import os, sys

def main():
    if len(sys.argv) < 2:
        print >> sys.stderr, "Usage: %s <pkg-list>" % sys.argv[0]
        sys.exit(1)
    pkgs = []
    for line in open(sys.argv[1]).readlines():
        for pkg in line.strip().split():
            if pkg.startswith('#'):
                break
            pkgs.append(pkg)

    sys.exit(
        os.system('sudo apt-get install %s' % ' '.join(pkgs)))


if __name__ == "__main__":
    main()
