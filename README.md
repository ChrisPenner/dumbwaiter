# Dumbwaiter

A 'dumb server' configured via simple yaml DSL. Useful when you just need a server to mock out some reponses for
testing or what have you.

Quickstart
==========

Save the following config into `server.yaml` and run it with
`dumbwaiter -f server.yaml --port 8080`

```yaml
# List your routes
# Each route has a matcher and a response
routes:
  # Specify requirements to match on
  # ALL requirements specified must match for the response to be triggered
  - match:
      path: /hello
    # We can also specify rules for the response
    # Here we're setting a simple body, responses default to a 200 status code
    response:
      body: Hello, World!

  # Here's another route matcher for a JSON request
  - match:
      # This matches ALL post requests
      method: post
    response:
      body: '{ result: "ok" }'
      # We can set headers as a yaml map
      headers:
        content-type: application/json

  # This is a catchall for 404s; this is provided for you by default
  # but I'll show you what it looks like:
  - match:
      path: .*
    response:
      body: Not Found
      status: 404
```
