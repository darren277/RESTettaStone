""""""
import random
import string


def generate_random_characters(line_length: int):
    return ''.join(random.choices(string.ascii_letters + string.digits + '/' + '+', k=line_length))

def generate_random_byte_characters(line_length: int):
    return ''.join(random.choices('abcdef' + string.digits, k=line_length))

def generate_lines(line_count: int, line_length: int):
    return [generate_random_characters(line_length) for _ in range(line_count)]


def test():
    s = "PuTTY-User-Key-File-2: ssh-rsa\nEncryption: none\nComment: imported-openssh-key\n"

    s += 'Public-Lines: 6\n'

    lines = generate_lines(6, 65)

    print(f"Generated {len(lines)} lines...")

    for i, line in enumerate(lines):
        if i == 5:
            s += line[:-13] + '\n'
        else:
            s += line + '\n'

    lines = generate_lines(14, 65)

    s += 'Private-Lines: 14\n'

    print(f"Generated {len(lines)} lines...")

    for i, line in enumerate(lines):
        if i == 13:
            s += line[:-18] + '=\n'
        else:
            s += line + '\n'

    private_mac = generate_random_byte_characters(40)

    s += f'Private-MAC: {private_mac}\n'

    return s

def test_rsa():
    s = "-----BEGIN RSA PRIVATE KEY-----\n"

    lines = generate_lines(21, 77)

    for i, line in enumerate(lines):
        if i == 20:
            s += line[:-6] + '=\n'
        else:
            s += line + '\n'

    s += '-----END RSA PRIVATE KEY-----\n'

    return s


if __name__ == '__main__':
    s = test()
    print(s)

    s = test_rsa()
    print(s)
