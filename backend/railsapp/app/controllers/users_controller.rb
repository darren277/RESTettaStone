class UsersController < ApplicationController
      rescue_from ActiveRecord::RecordNotFound, :with => :record_not_found

      def index
        users = User.all

        render json: { users: users }
      end

      def show
        render json: { user: user }
      end

      def create
        new_user = User.new(user_params)

        if new_user.save
          render json: { user: new_user }
        else
          render json: { errors: new_user.errors }, status: 500
        end
      end

      def update
        if user.update_attributes(user_params)
          render json: { user: user }
        else
          render json: { errors: user.errors }, status: 500
        end
      end

      def destroy
        if user.destroy!
          render json: { user: user }
        else
          render json: { errors: user.errors }, status: 500
        end
      end

      private

      def user_params
        params.require(:user).permit(:email)
      end

      def user
        @user ||= User.find_by!(id: id)
      end

      def id
        params.require(:id)
      end

      def record_not_found
        render json: { errors: ["Couldn't find user {id: #{id}}"] }, status: 500
      end
end
