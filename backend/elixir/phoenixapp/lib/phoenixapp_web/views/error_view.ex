defmodule PhoenixAppWeb.ErrorView do
  use PhoenixAppWeb, :view

  # Renders a 500 error response in JSON format
  def render("500.json", _assigns) do
    %{errors: %{detail: "Internal server error"}}
  end

  # Renders a 404 error response in JSON format
  def render("404.json", _assigns) do
    %{errors: %{detail: "Not found"}}
  end

  # Renders a 400 error response in JSON format
  def render("400.json", _assigns) do
    %{errors: %{detail: "Bad request"}}
  end

  # Catch-all for any other status codes like 422, 403, etc.
  def template_not_found(template, _assigns) do
    %{errors: %{detail: Phoenix.Controller.status_message_from_template(template)}}
  end
end
