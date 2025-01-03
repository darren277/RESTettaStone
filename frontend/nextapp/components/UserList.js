import UserItem from './UserItem'
import userStyles from '../styles/User.module.css'

const UserList = ({ users, onEdit, onDelete }) => {
    return (
        <div className={userStyles.grid}>
            {users.map((user) => (
                <div key={user.id}>
                    <UserItem user={user} />
                    <button onClick={() => onEdit(user)}>Edit</button>
                    <button onClick={() => onDelete(user.id)}>Delete</button>
                </div>
            ))}
        </div>
    )
}

export default UserList
