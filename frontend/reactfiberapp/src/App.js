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

  useEffect(() => {
    (async function () {
       const data = await fetchData();
       setUsers(data);
       console.log("USERS", data);
    }())
  }, []);

  if (users.length === 0) {
    return <Html center position={[-1, 1, -1]}><p>Loading...</p></Html>
  }

  return (
    <Html center position={[-1, 1, -1]}>
      <p>Click and drag on the white part to move the camera</p>
      <h3>Users</h3>
        {users.length > 0 &&
            <ul>
                {users.map((user) => (<li key={user.id}>{user.email}</li>))}
                <li>{JSON.stringify(users)}</li>
            </ul>
        }
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