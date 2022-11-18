local org = require("orgmode")
org.setup_ts_grammar()
org.setup {
    org_agenda_files = { "~/org/*"},
    org_default_notes_file = "~/org/inbox.org",
}
