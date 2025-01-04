defmodule PhoenixApp.Repo do
  use Ecto.Repo,
    otp_app: :phoenixapp,
    adapter: Ecto.Adapters.Postgres
end
