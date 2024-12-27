import { server } from '../config'
import UserList from '../components/UserList'
//import {middleware} from "../_middleware";
import {USER_API_BASE_URL} from "../config";

export default function Home({ users }) {
    return (
        <div>
            <UserList users={users} />
        </div>
    )
}

export const getStaticProps = async () => {
    const data = await fetch(`${USER_API_BASE_URL}/users`);
    const json = await data.json();

    return {
        props: {
            users: json,
        },
    }
}
