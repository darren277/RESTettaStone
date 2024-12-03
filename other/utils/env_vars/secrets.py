""""""

# SECRET_KEY=QCYtAnfkaZiwrNenxIlR6CTfG3gf90Latabg5241ABR5W1uDFNIkn
# LARAVEL_APP_KEY=base64:ugEgA/p+rIeiJrqW/UmHUT3h2hvJ+PnJnoMdO9MzPHY=

import random
import string


s = 'base64:ugEgA/p+rIeiJrqW/UmHUT3h2hvJ+PnJnoMdO9MzPHY='

def convert_base64(s):
    import base64

    return base64.b64decode(s.split(':')[1])

print(convert_base64(s))

def decode_bytes(b):
    return b.decode('windows-1252')

print(decode_bytes(convert_base64(s)))


import os

# Generate a 32-byte cryptographic random sequence
random_bytes32 = os.urandom(32)
print(random_bytes32.hex())  # Optional: View as a hex string

# Generate a 64-byte cryptographic random sequence
random_bytes64 = os.urandom(64)
print(random_bytes64.hex())  # Optional: View as a hex string


def convert_to_base64(s):
    import base64

    return base64.b64encode(s.encode()).decode()


converted_base64 = convert_to_base64(random_bytes32.hex())
print(converted_base64)



quit()


def generate_alphanum_secret(length=64):
    return ''.join(random.choices(string.ascii_letters + string.digits, k=length))

print(generate_alphanum_secret())


def generate_base64_secret(length=32):
    return 'base64:' + ''.join(random.choices(string.ascii_letters + string.digits, k=length))

print(generate_base64_secret())
