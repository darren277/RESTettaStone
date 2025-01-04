import Config

config :phoenixapp, PhoenixApp.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: System.get_env("PG_USER", "myusername"),
  password: System.get_env("PG_PASS", "mypassword"),
  database: System.get_env("PG_DB", "postgres"),
  hostname: System.get_env("PG_HOST", "localhost"),
  port: String.to_integer(System.get_env("PG_PORT", "5432")),
  pool_size: String.to_integer(System.get_env("PG_POOL_SIZE", "10"))

# log to console the connection string...
IO.puts("Database connection string: USER: #{System.get_env("PG_USER")} PASS: #{System.get_env("PG_PASS")} HOST: #{System.get_env("PG_HOST")} PORT: #{System.get_env("PG_PORT")} DB: #{System.get_env("PG_DB")}")

config :phoenixapp, PhoenixAppWeb.Endpoint,
  #url: [host: "localhost"],
  http: [ip: {0, 0, 0, 0}, port: String.to_integer(System.get_env("PHOENIXAPP_PORT", "4000"))],
  secret_key_base: System.get_env("PHOENIX_SECRET_KEY_BASE", "SOME_LONG_SECRET_KEY"),
  render_errors: [view: PhoenixAppWeb.ErrorView, accepts: ~w(json), layout: false],
  pubsub_server: PhoenixApp.PubSub,
  live_view: [signing_salt: "SALT"],
  server: true

config :logger, level: :info
config :phoenix, :json_library, Jason