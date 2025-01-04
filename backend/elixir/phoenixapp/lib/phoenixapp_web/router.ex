defmodule PhoenixAppWeb.Router do
  use PhoenixAppWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", PhoenixAppWeb do
    pipe_through :api

    resources "/users", UserController, except: [:new, :edit]
  end
end
