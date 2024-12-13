
const day07sample = `
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
`;

export function day07s() {
    day07(day07sample);
}

export function day07(data: string) {
    const parsed = data.trim().split('\n');
    const root = parseday07Dirs(parsed);

    const dirsList = flattenDirs(root);

    console.log('day07 a: ', resolve7a(dirsList));
    console.log('day07 b: ', resolve7b(root, dirsList));
}
function resolve7a(dirsList: Dir[]): number {
    return dirsList.map(dir => dir.filesize).filter(filesize => filesize <= 100000).sum();
}
function resolve7b(root: Dir, dirsList: Dir[]): number {
    const AVAILABLE = 70000000;
    const FREE = AVAILABLE - root.filesize;
    const REQUIRED = 30000000;
    const TO_FREE = REQUIRED - FREE;

    const sizes = dirsList.map(dir => dir.filesize).sort((a, b) => a - b);

    for (const size of sizes) {
        if (size >= TO_FREE) {
            return size;
        }
    }
}
function flattenDirs(dir: Dir): Dir[] {
    return Array.from(dir.dirs.values()).reduce((a, b) => a.concat(flattenDirs(b)), [dir]);
}
const CD_ROOT = /\$ cd \//;
const CD_UP = /\$ cd \.\./;
const CD_DIR = /\$ cd (\w+)/;
const LS = /\$ ls/;
const DIR = /dir (\w+)/;
const FILE = /(\d+) (\w+)\.?(\w+)?/;
function parseday07Dirs(lines: string[]): Dir {
    let m = null;

    const dir_stack: Dir[] = [new Dir('/')];

    for (const line of lines) {
        // console.log('-- dir stack:', dir_stack);
        // console.log('-- next cmd:', line);
        if (CD_ROOT.exec(line)) {
            if (dir_stack.length !== 1) {
                dir_stack.splice(1);
            }
        } else if (CD_UP.exec(line)) {
            dir_stack.pop();
        } else if (LS.exec(line)) {
            // skip
        } else if (m = CD_DIR.exec(line)) {
            const dir_name = m[1];
            const new_dir = dir_stack.last().getDir(dir_name);
            dir_stack.push(new_dir);
        } else if (m = DIR.exec(line)) {
            const dir_name = m[1];
            dir_stack.last().addDir(dir_name);
        } else if (m = FILE.exec(line)) {
            const [_, file_size, file_name, file_ext] = m;
            dir_stack.last().addFile(file_name, file_ext, parseInt(file_size, 10));
        }
    }

    return dir_stack[0];
}
class Dir {
    public dirs = new Map<string, Dir>();
    public files = new Map<string, File>();

    constructor(public name: string) {
    }

    get filesize(): number {
        let sum = 0;
        for (const dir of this.dirs.values()) {
            sum += dir.filesize;
        }
        for (const file of this.files.values()) {
            sum += file.filesize;
        }
        return sum;
    }

    addDir(name: string) {
        const dir = new Dir(name);
        this.dirs.set(dir.name, dir);
    }
    addFile(file_name: string, file_ext: string, file_size: number) {
        const file = new File(file_name, file_ext, file_size);
        this.files.set(file.name, file);
    }

    getDir(name: string): Dir {
        return this.dirs.get(name);
    }
}
class File {
    constructor(public namepart: string, public ext: string, public filesize: number) {
    }

    get name(): string {
        return this.namepart + '.' + this.ext;
    }
}
