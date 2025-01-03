import Link from 'next/link'
import { useRouter } from 'next/router'
import Meta from '../../../components/Meta'
import {USER_API_BASE_URL} from "../../../config";

const user = ({ user }) => {
    // const router = useRouter()
    // const { id } = router.query

    return (
        <>
            <Meta title={user.id} description={user.email} />
            <h1>{user.id}</h1>
            <p>{user.email}</p>
            <br />
            <Link href='/'>Go Back</Link>
        </>
    )
}

export const getStaticProps = async (context) => {
    const data = await fetch(`${USER_API_BASE_URL}/users`);
    const json = await data.json();

    return {
        props: {
            user: json[0]
        }
    }
}

export const getStaticPaths = async () => {
    const data = await fetch(`${USER_API_BASE_URL}/users`);
    const json = await data.json();

    const users = json;

    const ids = users.map((user) => user.id)
    const paths = ids.map((id) => ({ params: { id: id.toString() } }))

    return {
        paths,
        fallback: false
    }
}

export default user
