# The session resolves its awk executable once, here, so that no test reports
# its divergences from R at whichever read comes first.
invisible(suppressMessages(get_awk()))
