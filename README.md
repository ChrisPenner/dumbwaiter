# Dumbwaiter

An 'unintelligent server' (aka dumb-waiter) configured via simple yaml DSL.
Useful when you just need a server to mock out some reponses for testing or
what have you.

[Download binaries!](https://github.com/ChrisPenner/dumbwaiter/releases/latest)

Or `brew install ChrisPenner/tools/dumbwaiter`

Quickstart
==========

Save the following config into `server.yaml` and run it with
`dumbwaiter -f server.yaml --port 8080`

The server will watch for changes in your config file and will reload config changes
automatically.

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

  - match:
      path: /
    response:
      # Serve contents from a file
      # Filepath is relative to working directory of the running server
      file: ./index.html

  # Here's another route matcher for a JSON request
  - match:
      # This matches ALL post requests
      method: post
    response:
      body: '{ "result": "ok" }'
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

## Installation

On mac:
- `brew install ChrisPenner/tools/dumbwaiter`

On Linux (or Mac):
- Go to the [latest release](https://github.com/ChrisPenner/dumbwaiter/releases/latest) and download a binary

# Features

## Builtin Matchers

- `path`: Matches when provided regex matches the path of the request
    - e.g. `path: /users/[a-zA-Z]+`
- `method`: Matches when the http method matches the provided value. It's case insensitive.
    - e.g. `method: GET`

## Builtin Responders

- `body`: Appends the given string to the body of the response
    - e.g. `body: Hello World!`
- `status`: Sets the status code of the response to the given integer
    - e.g. `status: 401`
- `header`: Adds the given headers to the response
    - e.g. `headers: {"content-type": "application/json"}`
- `file`: Reads the given filepath and appends its contents to the response body. Filepath is relative to the 
          server's working directory
    - e.g. `file: ./src/index.html`

# Extensibility

Dumbwaiter is written in a compositional style; there are two concepts to know, `Matchers` and `Responders`

## [Matchers](./src/Dumbwaiter/Matchers.hs)

Matchers are used to determine whether a given *request* matches a *route*. *Matchers* are functions which when given
a request and `match` config respond with either `True` (the request matches this matcher) or `False` (the request does
not match this matcher). *Matchers* are *NOT* tied to a specific key in the `match` dictionary, they can use any keys
they like.

If any matcher rejects, then the whole route rejects. All matchers must match for a route to be triggered. This means
that if a matcher can determine that the user hasn't specified anything for that matcher, it should just return `True`
by default.

Examples:
- Path matcher: Checks the `path` key and does a regex match against the request's path. Returns `True` if the path
    matches the pattern *OR* if the `path` key is not provided
- Method matcher: Checks the `method` key and checks that it equals the request's method. Returns `True` if the method
    matches *OR* if the `method` key is not provided

Matchers are easy to write; feel free to fork and add your own, or better yet make a Pull Request and contribute them
here! Here's the [source code for matchers](./src/Dumbwaiter/Matchers.hs).

Here are some ideas:
- Only match if a query param is present
- Match on headers (e.g. content-type)

## [Responders](./src/Dumbwaiter/Responders.hs)

Responders are used to generate the *response* for route once the route has been matched. Responders work a bit like
middleware. A responder is a function which takes a `response` config (from the matching route) and returns a function
which operates on a ResponseBuilder and returns a new ResponseBuilder within the context of a *Firefly* `Handler`.
This means that a Responder can interact with or overwrite data provided by other responders. The Order of responders
matters in certain cases; but usually should not interfere with one another. If a responder doesn't have the context
to do something, or hasn't been configured in the `response` block it should just return the ResponseBuilder it is
passed.

Examples:
- Body responder: Appends any content in the `body` tag of the `response` to any existing body content in the response.
- Status responder: Sets the status code of the response to the code in the `response` config's `status` key.
- Header responder: Appends the headers from the map found in the `header` key of the `response`.

Just like *Matchers* it's easy to write your own *Responders*. Feel free to contribute some!

Here's some ideas:
- Generate response body from a provided *shell command*
- Transform request body into response by some *shell command*
- Extract a query param from the request and respond with it
- Choose response code based on whether request matches some parameters

Here's the [source code for responders](./src/Dumbwaiter/Responders.hs).
