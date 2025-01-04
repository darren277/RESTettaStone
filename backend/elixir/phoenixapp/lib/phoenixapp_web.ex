defmodule PhoenixAppWeb do
  def controller do
    quote do
      use Phoenix.Controller, namespace: PhoenixAppWeb

      import Plug.Conn
      alias PhoenixAppWeb.Router.Helpers, as: Routes
    end
  end

  def view do
    quote do
      use Phoenix.View,
        root: "lib/phoenixapp_web/templates",
        namespace: PhoenixAppWeb

      # Import convenience functions from controllers
      import Phoenix.Controller, only: [get_flash: 1, view_module: 1]

      alias PhoenixAppWeb.Router.Helpers, as: Routes
    end
  end

  def router do
    quote do
      use Phoenix.Router
    end
  end

  # Optionally define other macros for channels, etc.
  # def channel do
  #   quote do
  #     use Phoenix.Channel
  #   end
  # end

  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
