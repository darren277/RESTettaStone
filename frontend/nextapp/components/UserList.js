import UserItem from './UserItem'
import userStyles from '../styles/User.module.css'

const UserList = ({ users }) => {
    return (
        <div className={userStyles.grid}>
            {users.map((user) => (
                <UserItem user={user} />
            ))}
        </div>
    )
}

export default UserList
