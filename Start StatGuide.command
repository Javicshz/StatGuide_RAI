#!/bin/bash
# Double-click this file to open StatGuide R AI in your web browser (Mac).
# Keep the Terminal window open while you use the app. Close it to stop the app.

cd "$(dirname "$0")" || exit 1

RSCRIPT="$(command -v Rscript)"
for candidate in /usr/local/bin/Rscript /opt/homebrew/bin/Rscript /Library/Frameworks/R.framework/Resources/bin/Rscript; do
  if [ -z "$RSCRIPT" ] && [ -x "$candidate" ]; then
    RSCRIPT="$candidate"
  fi
done

if [ -z "$RSCRIPT" ]; then
  echo "R is not installed yet."
  echo "Download R from https://cran.r-project.org, install it, then double-click this file again."
  read -r -p "Press Enter to close this window."
  exit 1
fi

echo "Starting StatGuide R AI. Your browser will open in a moment."
echo "Keep this window open while you use the app. Close it to stop the app."
"$RSCRIPT" app.R
