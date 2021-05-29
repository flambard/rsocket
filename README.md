# rsocket

An implementation of the RSocket protocol for Erlang/OTP.

## Architecture

```
  |                            APPLICATION                            |
  |             inbound             |            outbound             |
  |                ^                |                v                |
  |      App:handle_request_n/2     |                v                |
  |      App:handle_payload/3       |                v                |
--+--- (behaviour implementation) --+--- (RSocket Application API) ---+--
  |      rsocket_channel            |        rsocket:request_*        |
  |      rsocket_request_stream     |                v                |
  |      rsocket_response_stream    |                v                |
R |                ^                |                v                |
  |                                                                   |
S |                          rsocket_stream                           |
  |                           (gen_server)                            |
O |                                                                   |
  |                ^                |                v                |
C | rsocket_stream:recv_request_n/2 |                v                |
  | rsocket_stream:recv_payload/3   |                v                |
K +--------------- ^ ---------------+--------------- v ---------------+
  |                ^                |    rsocket_connection:send_*    |
E |                ^                |                v                |
  |                                                                   |
T |                        rsocket_connection                         |
  |                           (gen_statem)                            |
  |                                                                   |
  |                ^                |                v                |
  | rsocket_transport:recv_frame/2  |        rsocket_transport        |
--+---- (RSocket Transport API) ----+--- (behaviour implementation) --+-
  |                ^                |      Transport:send_frame/2     |
  |                ^                |                v                |
  |                ^            TRANSPORT            v                |
  |                ^                |                v                |
--+---------------------------------+---------------------------------+--
  |                ^             NETWORK             v                |
```
