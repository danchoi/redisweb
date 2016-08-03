# redisweb

Simple configurable webapp to expose a Redis queue to a POST
action.

```
redisweb

Usage: redisweb [-v] PORT QUEUE PATH [-F separator] [-h IP-HEADER-NAME]
                PARAM-NAME

Available options:
  -h,--help                Show this help text
  -v                       Debug mode
  QUEUE                    Redis queue name
  PATH                     HTTP PATH for POST request
  -F separator             Separator character for queue message fields. Default
                           TAB
  -h IP-HEADER-NAME        Request header with IP address to log. E.g.
                           X-Real-IP. Default is to use REMOTE-HOST
  PARAM-NAME               Post param field name
```
