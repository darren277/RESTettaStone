defmodule PhoenixAppWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :phoenixapp

  ###plug Phoenix.PubSub.PG2 # or optional if you use PubSub
  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head

  plug PhoenixAppWeb.Router
end
