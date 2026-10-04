#!/usr/bin/env bash
# cloud/setup.sh: provision a Claude Code cloud environment for nansenbiomass
#
# What it does
#   1. Installs R from CRAN's Ubuntu repository (noble-cran40).
#   2. Installs the runtime libraries that the precompiled sf and terra binaries
#      link against (GDAL, GEOS, PROJ, udunits), and, at the same time, the R
#      tooling packages (renv, testthat, roxygen2, yaml).
#   3. Installs the spatial packages (sf, terra, sdmTMB) as precompiled Linux
#      binaries from Posit Package Manager (P3M), and configures R so that later
#      installs, including renv::restore(), also use binaries.
#   4. Installs the StoX R packages (RstoxData, RstoxBase, RstoxFramework) at pinned
#      versions from the StoX repository: their dependencies as P3M binaries, the
#      three packages from source (RstoxData contains C++ code).
#   It no longer restores renv.lock: cloud sessions switch renv's autoloader off
#   (.claude/settings.json), so the restored library was never used, and the
#   restore failed at igraph (docs/m0-acceptance.md, section 3).
#
# Why it is lean
#   Everything except the StoX packages is installed as binaries, and no
#   development headers are installed: an earlier version that installed
#   libgdal-dev and r-base-dev (371 packages with their dependencies, against 135
#   for the runtime libraries) exceeded the roughly five-minute setup budget. The
#   base image already provides gcc, g++ and make, and R ships its own headers,
#   which is enough to compile RstoxData's C++ code; Fortran is not available.
#   Installing the three StoX packages from source took 97 s on 4 cores
#   (3 October 2026; RstoxData 47 s, RstoxBase 14 s, RstoxFramework 37 s).
#
# How to use it
#   Paste this file into the environment's "Setup script" field (cloud environment
#   menu in the session's title bar, then Edit). It runs as root on Ubuntu 24.04,
#   with the repository cloned, before Claude Code starts. If it finishes within
#   roughly five minutes the result is cached, and later sessions skip it; the
#   cache is rebuilt when the script or the allowed hosts change, and after about
#   seven days. A script that runs longer can make the session fail to start with a
#   generic error. A non-zero exit also stops the session from starting, so only
#   stages whose failure would leave R unusable (apt, R itself) are fatal. If R
#   packages cannot be installed, the script prints a diagnosis of the downloads
#   (the exact error and every redirect followed), names the missing packages in a
#   warning, and lets the session start so that the problem can be investigated.
#
# Environment variables (the environment's "Environment variables" field)
#   LANG=C.UTF-8   Starts R in a UTF-8 locale, so that R CMD check does not switch to
#                  en_US.UTF-8. A check run with it returned Status: OK. The script
#                  also generates en_US.UTF-8, so the check stays clean without it.
#
# Network allowlist (custom allowed domains, with the default list included)
#   archive.ubuntu.com, security.ubuntu.com  Ubuntu packages (in the default list)
#   cloud.r-project.org                      R itself and the CRAN apt signing key
#   p3m.dev                                  binary R packages (index and metadata)
#   rspm-sync.rstudio.com                    binary R package files: p3m.dev answers each
#                                            download with a redirect (HTTP 307) to this host
#   packagemanager.posit.co                  former P3M address, still used by some tools
#   stoxproject.github.io                    StoX R packages (from M2)
#   Needed only from later milestones:
#   github.com, api.github.com,
#   codeload.github.com                      sdmTMBexperiments from GitHub (M3; default list)
#
# Timing
#   Each stage prints its elapsed time. On the first runs, R 4.6.1 installed in
#   12 s and the spatial libraries installed without error, but every R package
#   download from P3M failed although P3M's package index was read. The diagnosis
#   below found the cause: downloads are redirected to rspm-sync.rstudio.com, which
#   was not in the allowlist. With that host added (run 3, 28 September 2026), the
#   whole session, setup, build, check and tests, finished in under two minutes.
#   Stage timings go to the script's standard output, which the session itself
#   cannot see; the logs in /tmp/nansenbiomass-setup hold the install output only.
#   Logs older than the session are normal when the environment's cached image was
#   built by this script: the cache is rebuilt only when the stored script or the
#   allowed hosts change. To tell whether the current script built it, check that
#   the logs include the stages it writes (r-stox-deps.log and r-stox.log since
#   M2) and that the pinned versions are installed (docs/m2-plan.md, 4 October 2026). On 28 September 2026, changes saved to an
#   existing environment did not reach new sessions (runs 4 to 6); a newly created
#   environment ran the current script on its first session and passed with
#   Status: OK (docs/m0-acceptance.md).
#
# Data
#   This script installs software only. It reads no data and must never be
#   extended to fetch any (CLAUDE.md; docs/spec.md, Section 3).

set -euo pipefail
export DEBIAN_FRONTEND=noninteractive

UBUNTU_CODENAME="noble"
CRAN_APT="https://cloud.r-project.org/bin/linux/ubuntu"
P3M_REPO="https://p3m.dev/cran/__linux__/${UBUNTU_CODENAME}/latest"
# codetools is used by R CMD check for its code analysis.
TOOL_PACKAGES="renv testthat roxygen2 yaml codetools"
SPATIAL_PACKAGES="sf terra sdmTMB"
# StoX: the release used by the pipeline (StoX 4.2), pinned exactly. Change these
# together with DESCRIPTION and docs/m2-plan.md, never one alone.
STOX_REPO="https://stoxproject.github.io/repo/src/contrib"
STOX_PACKAGES="RstoxData_2.2.1 RstoxBase_2.2.1 RstoxFramework_4.2.1"
# Their CRAN dependencies, installed as P3M binaries before the source builds.
STOX_DEPS="data.table Rcpp stringi units xml2 geojsonsf ggplot2 jsonlite lwgeom maps jsonvalidate ncdf4 scales semver"
# Runtime libraries only; the binaries from P3M are built against these.
SPATIAL_LIBS=(libgdal34t64 libgeos-c1t64 libproj25 libudunits2-0)

SUDO=""
if [ "$(id -u)" -ne 0 ]; then SUDO="sudo"; fi

LOG_DIR=/tmp/nansenbiomass-setup
mkdir -p "$LOG_DIR"

start=$SECONDS
stage() { printf '\n[setup %4ds] %s\n' "$((SECONDS - start))" "$*"; }

MISSING_PACKAGES=""
DIAGNOSED=""

# Installs those of the R packages named in $1 that are not yet installed, and
# fails if any is missing afterwards (install.packages() itself only warns).
install_r_packages() {
  R_PACKAGES="$1" Rscript -e '
    pkgs <- strsplit(Sys.getenv("R_PACKAGES"), " ", fixed = TRUE)[[1]]
    todo <- setdiff(pkgs, rownames(installed.packages()))
    if (length(todo) > 0) install.packages(todo)
    missing <- setdiff(pkgs, rownames(installed.packages()))
    if (length(missing) > 0) {
      stop("Not installed: ", paste(missing, collapse = ", "))
    }
  '
}

# Prints why R package downloads fail: the error R reports for one download, and
# every redirect curl follows (a redirect to a host outside the allowlist would
# explain a readable index with failing downloads). Runs once; never fails.
diagnose_downloads() {
  if [ -n "$DIAGNOSED" ]; then return 0; fi
  DIAGNOSED=1
  echo "--- Diagnosing R package downloads"
  if [ -n "${https_proxy:-${HTTPS_PROXY:-}}" ]; then echo "An HTTPS proxy is set."; fi
  LOG_DIR="$LOG_DIR" Rscript -e '
    cat("download.file.method:", format(getOption("download.file.method")), "\n")
    a <- tryCatch(available.packages(), error = function(e) {
      cat("available.packages() failed:", conditionMessage(e), "\n")
      NULL
    })
    if (is.null(a) || !"R6" %in% rownames(a)) quit(status = 0)
    url <- paste0(contrib.url(getOption("repos")), "/R6_", a["R6", "Version"], ".tar.gz")
    writeLines(url, file.path(Sys.getenv("LOG_DIR"), "test-url"))
    cat("Test download:", url, "\n")
    res <- tryCatch(
      {
        download.file(url, tempfile(), quiet = TRUE)
        "ok"
      },
      warning = function(w) paste("warning:", conditionMessage(w)),
      error = function(e) paste("error:", conditionMessage(e))
    )
    cat("download.file:", res, "\n")
  ' || true
  if [ -s "$LOG_DIR/test-url" ]; then
    echo "curl, following redirects (status lines, Location headers, errors):"
    curl -sS -L --max-redirs 5 -o /dev/null -D - \
      -A "R/4 R (4 x86_64-pc-linux-gnu x86_64 linux-gnu)" "$(cat "$LOG_DIR/test-url")" 2>&1 |
      grep -iE '^(HTTP/|location:|curl:)' || true
  fi
  echo "--- End of diagnosis"
}

# apt reads only Ubuntu's own archive and CRAN, so the third-party sources in the
# base image (PPAs, Docker) cannot fail the run under a narrow allowlist.
APT_DIR=/etc/apt/nansenbiomass.sources.d
APT_OPTS=(-o "Dir::Etc::SourceList=/dev/null" -o "Dir::Etc::SourceParts=${APT_DIR}")

stage "Configuring apt sources (Ubuntu archive and CRAN)"
$SUDO mkdir -p "$APT_DIR"
if [ -f /etc/apt/sources.list.d/ubuntu.sources ]; then
  $SUDO cp /etc/apt/sources.list.d/ubuntu.sources "$APT_DIR/ubuntu.sources"
else
  $SUDO cp /etc/apt/sources.list "$APT_DIR/ubuntu.list"
fi
curl -fsSL "${CRAN_APT}/marutter_pubkey.asc" |
  $SUDO tee /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc >/dev/null
echo "deb ${CRAN_APT} ${UBUNTU_CODENAME}-cran40/" |
  $SUDO tee "$APT_DIR/cran.list" >/dev/null

stage "Installing R"
$SUDO apt-get "${APT_OPTS[@]}" update -qq
# libxml2 is needed by the xml2 binary that roxygen2 depends on. locales provides
# locale-gen: R CMD check switches to en_US.UTF-8 when the session has no UTF-8
# locale set, and warns if that locale does not exist.
$SUDO apt-get "${APT_OPTS[@]}" install -y -qq --no-install-recommends r-base-core libxml2 locales
$SUDO locale-gen en_US.UTF-8
if ! locale -a | grep -qix 'en_US.utf8'; then
  echo "WARNING: the en_US.UTF-8 locale was not generated; set LANG=C.UTF-8 in the"
  echo "environment's variables, or R CMD check will warn about the locale."
fi
R --version | head -n 1

stage "Configuring R to install binary packages from P3M"
RPROFILE_SITE="$(R RHOME)/etc/Rprofile.site"
if ! grep -q 'nansenbiomass setup' "$RPROFILE_SITE" 2>/dev/null; then
  $SUDO tee -a "$RPROFILE_SITE" >/dev/null <<EOF

# nansenbiomass setup: binary packages from Posit Package Manager
local({
  options(
    repos = c(CRAN = "${P3M_REPO}"),
    # P3M serves Linux binaries only when the user agent names R and the platform
    HTTPUserAgent = sprintf(
      "R/%s R (%s)", getRversion(),
      paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
    ),
    Ncpus = max(1L, parallel::detectCores())
  )
})
EOF
fi
# renv::restore() otherwise uses the repositories recorded in renv.lock, which
# on a Windows laptop serve no Linux binaries.
RENVIRON_SITE="$(R RHOME)/etc/Renviron.site"
if ! grep -q 'RENV_CONFIG_REPOS_OVERRIDE' "$RENVIRON_SITE" 2>/dev/null; then
  echo "RENV_CONFIG_REPOS_OVERRIDE=${P3M_REPO}" | $SUDO tee -a "$RENVIRON_SITE" >/dev/null
fi

# The spatial libraries (apt) and the tooling packages (R) do not depend on each
# other, so they are installed at the same time.
stage "Installing spatial libraries and R tooling packages (${TOOL_PACKAGES}) in parallel"
$SUDO apt-get "${APT_OPTS[@]}" install -y -qq --no-install-recommends "${SPATIAL_LIBS[@]}" \
  >"$LOG_DIR/apt-spatial.log" 2>&1 &
apt_pid=$!
install_r_packages "$TOOL_PACKAGES" >"$LOG_DIR/r-tools.log" 2>&1 &
tools_pid=$!
apt_status=0
wait "$apt_pid" || apt_status=$?
tools_status=0
wait "$tools_pid" || tools_status=$?
if [ "$apt_status" -ne 0 ]; then
  echo "Installing the spatial libraries failed (apt exit $apt_status). Last log lines:"
  tail -n 30 "$LOG_DIR/apt-spatial.log"
  exit 1
fi
if [ "$tools_status" -ne 0 ]; then
  echo "Installing the R tooling packages failed. Last log lines:"
  tail -n 15 "$LOG_DIR/r-tools.log"
  MISSING_PACKAGES="$MISSING_PACKAGES $TOOL_PACKAGES"
  diagnose_downloads
fi

stage "Installing spatial R packages: ${SPATIAL_PACKAGES}"
if ! install_r_packages "$SPATIAL_PACKAGES" >"$LOG_DIR/r-spatial.log" 2>&1; then
  echo "Installing the spatial R packages failed. Last log lines:"
  tail -n 15 "$LOG_DIR/r-spatial.log"
  MISSING_PACKAGES="$MISSING_PACKAGES $SPATIAL_PACKAGES"
  diagnose_downloads
fi

stage "Installing StoX dependencies (binaries)"
if ! install_r_packages "$STOX_DEPS" >"$LOG_DIR/r-stox-deps.log" 2>&1; then
  echo "Installing the StoX dependencies failed. Last log lines:"
  tail -n 15 "$LOG_DIR/r-stox-deps.log"
  MISSING_PACKAGES="$MISSING_PACKAGES $STOX_DEPS"
  diagnose_downloads
fi

stage "Installing StoX packages from source: ${STOX_PACKAGES}"
for pkg in $STOX_PACKAGES; do
  if ! MAKEFLAGS="-j$(nproc)" STOX_URL="${STOX_REPO}/${pkg}.tar.gz" Rscript -e '
    install.packages(Sys.getenv("STOX_URL"), repos = NULL, type = "source")
    name <- sub("_.*$", "", basename(Sys.getenv("STOX_URL")))
    if (!requireNamespace(name, quietly = TRUE)) stop("Not installed: ", name)
  ' >>"$LOG_DIR/r-stox.log" 2>&1; then
    echo "Installing ${pkg} failed. Last log lines:"
    tail -n 15 "$LOG_DIR/r-stox.log"
    MISSING_PACKAGES="$MISSING_PACKAGES ${pkg%%_*}"
    break
  fi
done

stage "Checking the installation"
R_PACKAGES="$TOOL_PACKAGES $SPATIAL_PACKAGES RstoxData RstoxBase RstoxFramework" Rscript -e '
  pkgs <- strsplit(Sys.getenv("R_PACKAGES"), " ", fixed = TRUE)[[1]]
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      cat(sprintf("%-10s NOT INSTALLED\n", p))
      next
    }
    # Loading, not just finding, confirms that the binaries link correctly.
    cat(sprintf("%-10s %s\n", p, format(packageVersion(p))))
  }
  if (requireNamespace("sf", quietly = TRUE)) {
    print(sf::sf_extSoftVersion()[c("GEOS", "GDAL", "proj.4")])
  }
  if (requireNamespace("terra", quietly = TRUE)) {
    cat("terra linked to GDAL", terra::gdal(), "\n")
  }
' || echo "WARNING: the installation check itself failed; see the lines above."

# renv.lock is not restored here: cloud sessions switch renv's autoloader off, so
# R uses the packages installed above, and the lockfile governs the laptop
# (CLAUDE.md; docs/m0-acceptance.md, section 3).

stage "Done"
if [ -n "$MISSING_PACKAGES" ]; then
  echo ""
  echo "WARNING: R packages not installed:$MISSING_PACKAGES"
  echo "R itself works. The session starts anyway so that this can be investigated;"
  echo "see the diagnosis above and the logs in $LOG_DIR."
fi
