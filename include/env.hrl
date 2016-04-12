-define(ENV, prod).

-define(TRANSPORT, ranch_tcp).
-define(PROTOCOL, tcp).
-define(PROTOCOL_CLOSE, tcp_closed).
-define(PROTOCOL_ERROR, tcp_error).

-ifndef(DEF_PORT).
-define(DEF_PORT, 7777).
-endif.