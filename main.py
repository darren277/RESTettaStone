import sys
print(sys.argv)

if len(sys.argv) < 2:
    print("Please provide a minimum of one argument.")
    sys.exit(1)

if sys.argv[1] == 'tests':
    from other.tests.main import run_tests
    run_tests()
