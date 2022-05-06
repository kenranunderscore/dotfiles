certPath:

#
# weechat -- irc.conf
#
# WARNING: It is NOT recommended to edit this file by hand,
# especially if WeeChat is running.
#
# Use /set or similar command to change settings in WeeChat.
#
# For more info, see: https://weechat.org/doc/quickstart
#

''
  [look]
  buffer_open_before_autojoin = on
  buffer_open_before_join = off
  buffer_switch_autojoin = on
  buffer_switch_join = on
  color_nicks_in_names = off
  color_nicks_in_nicklist = off
  color_nicks_in_server_messages = on
  color_pv_nick_like_channel = on
  ctcp_time_format = "%a, %d %b %Y %T %z"
  display_away = local
  display_ctcp_blocked = on
  display_ctcp_reply = on
  display_ctcp_unknown = on
  display_host_join = on
  display_host_join_local = on
  display_host_quit = on
  display_join_message = "329,332,333,366"
  display_old_topic = on
  display_pv_away_once = on
  display_pv_back = on
  display_pv_warning_address = off
  highlight_channel = "$nick"
  highlight_pv = "$nick"
  highlight_server = "$nick"
  highlight_tags_restrict = "irc_privmsg,irc_notice"
  item_channel_modes_hide_args = "k"
  item_display_server = buffer_plugin
  item_nick_modes = on
  item_nick_prefix = on
  join_auto_add_chantype = off
  msgbuffer_fallback = current
  new_channel_position = none
  new_pv_position = none
  nick_completion_smart = speakers
  nick_mode = prefix
  nick_mode_empty = off
  nicks_hide_password = "nickserv"
  notice_as_pv = auto
  notice_welcome_redirect = on
  notice_welcome_tags = ""
  notify_tags_ison = "notify_message"
  notify_tags_whois = "notify_message"
  part_closes_buffer = off
  pv_buffer = independent
  pv_tags = "notify_private"
  raw_messages = 256
  server_buffer = merge_with_core
  smart_filter = on
  smart_filter_account = on
  smart_filter_chghost = on
  smart_filter_delay = 5
  smart_filter_join = on
  smart_filter_join_unmask = 30
  smart_filter_mode = "+"
  smart_filter_nick = on
  smart_filter_quit = on
  temporary_servers = off
  topic_strip_colors = off

  [color]
  input_nick = lightcyan
  item_channel_modes = default
  item_lag_counting = default
  item_lag_finished = yellow
  item_nick_modes = default
  message_account = cyan
  message_chghost = brown
  message_join = green
  message_kick = red
  message_quit = red
  mirc_remap = "1,-1:darkgray"
  nick_prefixes = "y:lightred;q:lightred;a:lightcyan;o:lightgreen;h:lightmagenta;v:yellow;*:lightblue"
  notice = green
  reason_kick = default
  reason_quit = default
  topic_current = default
  topic_new = white
  topic_old = default

  [network]
  autoreconnect_delay_growing = 2
  autoreconnect_delay_max = 600
  ban_mask_default = "*!$ident@$host"
  colors_receive = on
  colors_send = on
  lag_check = 60
  lag_max = 1800
  lag_min_show = 500
  lag_reconnect = 300
  lag_refresh_interval = 1
  notify_check_ison = 1
  notify_check_whois = 5
  sasl_fail_unavailable = on
  send_unknown_commands = off
  whois_double_nick = off

  [msgbuffer]

  [ctcp]

  [ignore]

  [server_default]
  addresses = ""
  anti_flood_prio_high = 2
  anti_flood_prio_low = 2
  autoconnect = off
  autojoin = ""
  autoreconnect = on
  autoreconnect_delay = 10
  autorejoin = off
  autorejoin_delay = 30
  away_check = 0
  away_check_max_nicks = 25
  capabilities = ""
  charset_message = message
  command = ""
  command_delay = 0
  connection_timeout = 60
  default_chantypes = "#&"
  ipv6 = on
  local_hostname = ""
  msg_kick = ""
  msg_part = "WeeChat ${"info:version"}"
  msg_quit = "WeeChat ${"info:version"}"
  nicks = "kenran,kenran1,kenran2,kenran3,kenran4"
  nicks_alternate = on
  notify = ""
  password = ""
  proxy = ""
  realname = ""
  sasl_fail = continue
  sasl_key = ""
  sasl_mechanism = plain
  sasl_password = ""
  sasl_timeout = 15
  sasl_username = ""
  split_msg_max_length = 512
  ssl = off
  ssl_cert = ""
  ssl_dhkey_size = 2048
  ssl_fingerprint = ""
  ssl_password = ""
  ssl_priorities = "NORMAL:-VERS-SSL3.0"
  ssl_verify = on
  usermode = ""
  username = "kenran"

  [server]
  liberachat.addresses = "irc.libera.chat/6697"
  liberachat.proxy
  liberachat.ipv6
  liberachat.ssl = on
  liberachat.ssl_cert = "${certPath}"
  liberachat.ssl_password
  liberachat.ssl_priorities
  liberachat.ssl_dhkey_size
  liberachat.ssl_fingerprint
  liberachat.ssl_verify = on
  liberachat.password
  liberachat.capabilities
  liberachat.sasl_mechanism
  liberachat.sasl_username
  liberachat.sasl_password
  liberachat.sasl_key
  liberachat.sasl_timeout
  liberachat.sasl_fail
  liberachat.autoconnect = on
  liberachat.autoreconnect
  liberachat.autoreconnect_delay
  liberachat.nicks
  liberachat.nicks_alternate
  liberachat.username
  liberachat.realname
  liberachat.local_hostname
  liberachat.usermode
  liberachat.command
  liberachat.command_delay
  liberachat.autojoin = "#racket,#linux,#emacs,#org-mode,#haskell,#nixos,#zig,#nim,#nyxt,##crawl"
  liberachat.autorejoin
  liberachat.autorejoin_delay
  liberachat.connection_timeout
  liberachat.anti_flood_prio_high
  liberachat.anti_flood_prio_low
  liberachat.away_check
  liberachat.away_check_max_nicks
  liberachat.msg_kick
  liberachat.msg_part
  liberachat.msg_quit
  liberachat.notify
  liberachat.split_msg_max_length
  liberachat.charset_message
  liberachat.default_chantypes
''
