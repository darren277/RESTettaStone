defmodule PhoenixApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :phoenixapp,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Tells Mix how to start your application
  def application do
    [
      mod: {PhoenixApp.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  # Specifies your project dependencies
  defp deps do
    [
      {:phoenix, "~> 1.7"},
      {:phoenix_view, "~> 2.0"},
      {:phoenix_pubsub, "~> 2.1"},
      {:phoenix_ecto, "~> 4.4"},
      {:ecto_sql, "~> 3.10"},
      {:postgrex, ">= 0.0.0"},
      {:plug_cowboy, "~> 2.5"},
      # (ArgumentError) invalid :json_decoder option. The module Jason is not loaded and could not be found
      {:jason, "~> 1.2"},
    ]
  end
end
