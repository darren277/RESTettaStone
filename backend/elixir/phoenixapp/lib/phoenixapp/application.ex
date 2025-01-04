defmodule PhoenixApp.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      # Start the Ecto repository
      PhoenixApp.Repo,
      # Start the PubSub system (optional, but Phoenix uses it by default)
      {Phoenix.PubSub, name: PhoenixApp.PubSub},
      # Start the Endpoint (http/https)
      PhoenixAppWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html for other strategies
    opts = [strategy: :one_for_one, name: PhoenixApp.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    PhoenixAppWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
