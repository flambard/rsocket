# rsocket

An implementation of the RSocket protocol for Erlang/OTP.

## Architecture

```
  |                            APPLICATION                            |
  |             inbound             |            outbound             |
  |                ^                |                v                |
  |      App:handle_request_n/2     |                v                |
  |      App:handle_payload/3       |                v                |
==+== (behaviour implementation) ===+=== (RSocket Application API) ===+==
  |      rsocket_channel            |        rsocket:request_*        |
  |      rsocket_request_stream     +--------------- v ---------------+
  |      rsocket_response_stream    |     rsocket_stream:request_*    |
  |                ^                |                v                |
R |                                                                   |
  |                          rsocket_stream                           |
S |                           (gen_server)                            |
  |                                                                   |
O |                ^                |                v                |
  | rsocket_stream:recv_request_n/2 |                v                |
C | rsocket_stream:recv_payload/3   |                v                |
  +--------------- ^ ---------------+--------------- v ---------------+
K |                ^                |    rsocket_connection:send_*    |
  |                ^                |                v                |
E |                                                                   |
  |                        rsocket_connection                         |
T |                           (gen_statem)                            |
  |                                                                   |
  |                ^                |                v                |
  | rsocket_connection:recv_frame/2 |                v                |
  +--------------- ^ ---------------+                v                |
  | rsocket_transport:recv_frame/2  |        rsocket_transport        |
==+==== (RSocket Transport API) ====+=== (behaviour implementation) ==+==
  |                ^                |      Transport:send_frame/2     |
  |                ^                |                v                |
  |                ^            TRANSPORT            v                |
  |                ^                |                v                |
--+---------------------------------+---------------------------------+--
  |                ^             NETWORK             v                |
```
