import { server } from '../config'
import UserList from '../components/UserList'
//import {middleware} from "../_middleware";
import {RUN_TIME_USER_API_BASE_URL} from "../config";
import {useEffect, useState} from "react";

//const API_ROUTE = '/api';
const API_ROUTE = RUN_TIME_USER_API_BASE_URL;

export default function Home() {
    const [users, setUsers] = useState([]);
    const [newUser, setNewUser] = useState('');
    const [editingUser, setEditingUser] = useState(null);

    useEffect(() => {
        fetchUsers();
    }, []);

    const fetchUsers = async () => {
        const res = await fetch(`${API_ROUTE}/users`);
        const data = await res.json();
        setUsers(data);
    };

    const createUser = async () => {
        if (!newUser) {
            alert('Email is required');
            return;
        }

        const res = await fetch(`${API_ROUTE}/users`, {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({ email: newUser }),
        });

        const user = await res.json();
        setUsers([...users, user]);
        setNewUser('');
    };

    const updateUser = async (id, email) => {
        const res = await fetch(`${API_ROUTE}/users/${id}`, {
            method: 'PUT',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({ email }),
        });

        const updatedUser = await res.json();
        setUsers(users.map(user => (user.id === id ? updatedUser : user)));
        setEditingUser(null);
    };

    const deleteUser = async id => {
        await fetch(`${API_ROUTE}/users/${id}`, {method: 'DELETE'});

        setUsers(users.filter(user => user.id !== id));
    };

    return (
        <div>
            <h1>Users</h1>

            <div>
                <input
                    type="email"
                    value={newUser}
                    onChange={e => setNewUser(e.target.value)}
                    placeholder="Enter email"
                />
                <button onClick={createUser}>Add User</button>
            </div>

            {editingUser && (
                <div>
                    <input
                        type="email"
                        defaultValue={editingUser.email}
                        onBlur={e => updateUser(editingUser.id, e.target.value)}
                    />
                </div>
            )}

            <UserList users={users} onEdit={setEditingUser} onDelete={deleteUser} />
        </div>
    )
}

export const getStaticProps = async () => {
    const data = await fetch(`${API_ROUTE}/users`);
    const json = await data.json();

    return {
        props: {
            users: json,
        },
    }
}
