# rsocket

An implementation of the RSocket protocol for Erlang/OTP.

## Architecture

```
  |                    APPLICATION                     |
  |        inbound          |        outbound          |
  |           ^             |            v             |
  |           ^             |            v             |
--+--- (behaviour impl) ----+---- (Application API) ---+-
  | rsocket_channel         |         rsocket          |
  | rsocket_request_stream  |            v             |
R | rsocket_response_stream |            v             |
  |           ^             |            v             |
S |                                                    |
  |                  rsocket_stream                    |
O |                   (gen_server)                     |
  |                                                    |
C |           ^             |            v             |
  +----- (raw message) -----+--- rsocket_connection ---+
K |           ^             |            v             |
  |                                                    |
E |                rsocket_connection                  |
  |                   (gen_statem)                     |
T |                                                    |
  |           ^             |            v             |
  |    rsocket_transport    |    rsocket_transport     |
--+---- (Transport API) ----+---- (behaviour impl) ----+-
  |           ^             |            v             |
  |           ^         TRANSPORT        v             |
  |           ^             |            v             |
--+-------------------------+--------------------------+-
  |           ^          NETWORK         v             |
```
