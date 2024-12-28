import React, { useState, useEffect } from "react";
import { Canvas } from "@react-three/fiber";
import { DoubleSide } from "three";

import { Html, OrbitControls, PerspectiveCamera } from "@react-three/drei";

import UserService from "./UserService";

import "./style.css";


function fetchData() {
    return UserService.getUsers().then((response) => {
        return response.data;
    });
}


export default function App() {
    return (
        <div>
            <Canvas style={{ height: "100vh", width: "100vw" }}>
                <group>
                    <GreenSquare />
                    <ToolTipMenu />
                    <ToolTipDiv1 />
                    <ToolTipDiv2 />
                </group>
                <ambientLight />
                <PerspectiveCamera position={[2, 2, 2]} makeDefault />
                <OrbitControls />
            </Canvas>
        </div>
    );
}


function GreenSquare() {
    return (
        <mesh position={[0, 0, 0]} rotation={[Math.PI / 2, 0, 0]} scale={[1, 1, 1]}>
            <planeGeometry />
            <meshBasicMaterial color="green" side={DoubleSide} />
        </mesh>
    );
}


// Drei's Html component lets you render any HTML inside the 3d scene.
// It follows the same rules as everything else when it comes to positioning, but is not actually rendered inside the canvas
function ToolTipMenu() {
    const [users, setUsers] = useState([]);
    const [newEmail, setNewEmail] = useState("");
    const [editingUser, setEditingUser] = useState(null);

    useEffect(() => {
        fetchUsers();
    }, []);

    async function fetchUsers() {
        try {
            const data = await fetchData();
            setUsers(data);
        } catch (err) {
            console.error("Error fetching users:", err);
        }
    }

    async function handleCreateUser() {
        if (!newEmail) {
            alert("Email is required.");
            return;
        }

        try {
            const response = await UserService.createUser({ email: newEmail });
            setUsers([...users, response.data]);
            setNewEmail("");
        } catch (err) {
            console.error("Error creating user:", err);
        }
    }

    async function handleUpdateUser(userId, email) {
        if (!email) {
            alert("Email is required.");
            return;
        }

        try {
            await UserService.updateUser(userId, { email });
            setUsers(users.map((user) => (user.id === userId ? { ...user, email } : user)));
            setEditingUser(null);
        } catch (err) {
            console.error("Error updating user:", err);
        }
    }

    async function handleDeleteUser(userId) {
        try {
            await UserService.deleteUser(userId);
            setUsers(users.filter((user) => user.id !== userId));
        } catch (err) {
            console.error("Error deleting user:", err);
        }
    }

    if (users.length === 0) {
        return <Html center position={[-1, 1, -1]}><p>Loading...</p></Html>
    }

    return (
        <Html center position={[-1, 1, -1]}>
            <p>Click and drag on the white part to move the camera</p>
            <h3>Users</h3>
            <ul>
                {users.map((user) => (
                    <li key={user.id}>
                        {editingUser === user.id ? (
                            <input type="email" defaultValue={user.email} onBlur={(e) => handleUpdateUser(user.id, e.target.value)} />
                        ) : (
                            <>
                                {user.email}
                                <button onClick={() => setEditingUser(user.id)}>Edit</button>
                                <button onClick={() => handleDeleteUser(user.id)}>Delete</button>
                            </>
                        )}
                    </li>
                ))}
            </ul>
            <input type="email" placeholder="New user email" value={newEmail} onChange={(e) => setNewEmail(e.target.value)} />
            <button onClick={handleCreateUser}>Add User</button>
        </Html>
    );
}

function ToolTipDiv1() {
    return (
        <Html center position={[1, -1, -1]}>
            <p>Scroll to zoom in and out</p>
        </Html>
    );
}

function ToolTipDiv2() {
    return (
        <Html center position={[-1, -1, 1]}>
            <p>{"Words and more words."}</p>
        </Html>
    );
}