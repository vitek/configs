import argparse


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--input', required=True)
    parser.add_argument('-o', '--output', required=True)
    parser.add_argument('vars', nargs='*')

    args = parser.parse_args()

    substitutions = []
    for var in args.vars:
        key, value = var.split('=', 1)
        substitutions.append((f'@{key}@', value))

    with open(args.input) as fp:
        data = fp.read()

    for key, value in substitutions:
        data = data.replace(key, value)

    with open(args.output, 'w') as fp:
        fp.write(data)


if __name__ == '__main__':
    main()
