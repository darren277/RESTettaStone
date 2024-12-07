class UsersController < ApplicationController
  before_action :set_user, only: [:show, :update, :destroy]
  rescue_from ActiveRecord::RecordNotFound, with: :record_not_found

  def index
    users = User.all
    render json: users
  end

  def show
    render json: { user: @user }
  end

  def create
    new_user = User.new(user_params)
    if new_user.save
      render json: { user: new_user }, status: 200
    else
      render json: { errors: new_user.errors }, status: 422
    end
  end

  def update
    if @user.update(user_params)
      render json: { user: @user }
    else
      render json: { errors: @user.errors }, status: 422
    end
  end

  def destroy
    @user.destroy
    render json: { message: "User successfully deleted." }, status: :ok
  end

  private

  def set_user
    @user = User.find(params[:id])
  end

  def user_params
    params.permit(:email)
  end

  def record_not_found
    render json: { errors: ["Couldn't find user with id: #{params[:id]}"] }, status: 404
  end
end
