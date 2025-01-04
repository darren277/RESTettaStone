defmodule PhoenixAppWeb.UserView do
  use PhoenixAppWeb, :view

  def render("index.json", %{users: users}) do
    Enum.map(users, fn user ->
      %{
        id: user.id,
        email: user.email
      }
    end)
  end

  def render("show.json", %{user: user}) do
    render_one(user, __MODULE__, "user.json")
  end

  def render("error.json", %{changeset: changeset}) do
    %{errors: translate_errors(changeset)}
  end

  def render("user.json", %{user: user}) do
    %{id: user.id, email: user.email}
  end

  defp translate_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, _opts} -> msg end)
  end
end
