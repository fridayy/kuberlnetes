
-type maybe(T) :: T | undefined.

-record(auth_token, {token :: binary()}).
-record(auth_cert, {cert :: binary(), key :: binary()}).
-record(server, {
    url :: string(),
    host :: string(),
    port :: integer(),
    ca_cert :: binary() | undefined,
    auth :: #auth_token{} | #auth_cert{},
    skip_tls_verify :: boolean(),
    username :: maybe(string()),
    clustername :: maybe(string())
}).
