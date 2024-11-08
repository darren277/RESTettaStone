import headerStyles from '../styles/Header.module.css'

const Header = () => {
  return (
    <div>
      <h1 className={headerStyles.title}>
        Next App
      </h1>
      <p className={headerStyles.description}>
        This is the Next app that fetches users from the many back end APIs.
      </p>
    </div>
  )
}

export default Header
