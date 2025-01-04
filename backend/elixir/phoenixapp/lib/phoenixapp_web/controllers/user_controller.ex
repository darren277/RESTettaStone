defmodule PhoenixAppWeb.UserController do
  use PhoenixAppWeb, :controller

  alias PhoenixApp.Repo
  alias PhoenixApp.Schemas.User

  # GET /users
  def index(conn, _params) do
    users = Repo.all(User)
    render(conn, "index.json", users: users)
  end

  # POST /users
  def create(conn, %{"email" => email}) do
    changeset = User.changeset(%User{}, %{"email" => email})

    case Repo.insert(changeset) do
      {:ok, user} ->
        conn
        #|> put_status(:created)
        |> put_status(:ok)
        |> render("show.json", user: user)

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render("error.json", changeset: changeset)
    end
  end

  # GET /users/:id
  def show(conn, %{"id" => id}) do
    user = Repo.get!(User, id)
    render(conn, "show.json", user: user)
  end

  # PUT /users/:id
  def update(conn, %{"id" => id, "email" => email}) do
    user = Repo.get!(User, id)
    changeset = User.changeset(user, %{"email" => email})

    case Repo.update(changeset) do
      {:ok, user} ->
        render(conn, "show.json", user: user)

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render("error.json", changeset: changeset)
    end
  end

  # DELETE /users/:id
  def delete(conn, %{"id" => id}) do
    user = Repo.get!(User, id)
    {:ok, _user} = Repo.delete(user)

    #send_resp(conn, :no_content, "")
    send_resp(conn, :ok, "")
  end
end
