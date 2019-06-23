# smap - a command line tool for sets and maps

## Installation:

Download this repo and run

```bash
stack install smap
```

You will need [stack](https://www.haskellstack.org) if you don't already have it. 

## Tutorial:

The setup:

```bash
cat > patients << EOF
Bob Smith
Jane Doe
John Smith
Carol Carell
EOF

cat > has_cold << EOF
Jane Doe
John Smith
EOF

cat > has_mumps << EOF
Jane Doe
Carol Carell
EOF
```

### Simple usage (sets)

#### cat - Set Union (and Deduplication)

Sick patients:

```bash
$ smap cat has_cold has_mumps
Jane Doe
John Smith
Carol Carell
```

You can also use `-` instead of a filename to represent stdin/stdout. (This works for any command.)

```bash
$ cat has_cold | smap cat - has_mumps
Jane Doe
John Smith
Carol Carell
```

If you don't provide any arguments, `cat` will assume you mean stdin.

```bash
$ cat has_cold has_mumps | smap cat
Jane Doe
John Smith
Carol Carell
```

#### sub - Set subtraction

Healthy patients:

```bash
$ smap sub patients has_cold has_mumps
Bob Smith
```

#### int - Set intersection

Patients with both a cold and mumps:

```bash
$ smap int has_cold has_mumps
Jane Doe
```

#### xor - Symmetric difference

Patients who only have a cold or mumps, but not both:

```bash
$ smap xor has_cold has_mumps
Carol Carell
John Smith
```

### Advanced usage (maps)

When using `smap` with sets, the behavior is pretty straightforward. It gets a bit more complicated when
dealing with maps.

If you provide `smap` with a filepath, it will construct a map where the keys equal the values. (This
is equivalent to a set). If you pass in `+file1,file2` 
as an argument, `smap` will construct a map using lines from file1 as keys and lines from file2 as values. 

We can get a list of patient last names using `cut -f 2 -d ' ' <patient file>`

#### Pick one patient from each family:

```bash
$ smap cat +<(cut -f 2 -d ' ' patients),patients
Bob Smith
Jane Doe
Carol Carell
```

To understand the above:

* `<(cut -f 2 -d ' ' patients)` gets a list of all the patients' last names and creates a virtual file with this list. See [bash process substitution](https://www.tldp.org/LDP/abs/html/process-sub.html).
* `+<(cut -f 2 -d ' ' patients),patients` constructs a stream where the keys are the last names and the values are the whole names.

`cat` deduplicates by key, so if we see a second (or third, or fourth, etc.) person from a given family we don't print them out.


#### Patients who have family members with a cold:

```bash
$ smap int +<(cut -f 2 -d ' ' patients),patients <(cut -f 2 -d ' ' has_cold)
Bob Smith
Jane Doe
John Smith
```

To understand the above:

* `<(cut -f 2 -d ' ' patients)` gets a list of all the patients' last names.
* `+<(cut -f 2 -d ' ' patients),patients` constructs a stream where the keys are the last names and the values are the whole names.
* `<(cut -f 2 -d ' ' has_cold)` gets a list of family names of everyone who has a cold.

So `int` is filtering the first argument (treated as a `key,value` stream) by the keys present in the second argument.
