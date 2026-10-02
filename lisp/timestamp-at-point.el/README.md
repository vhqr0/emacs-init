# timestamp-at-point

Convert the number at point to a readable time and echo it.  With `C-u`,
also copy it to the kill ring; with `C-u C-u`, also replace the number with
it.

## Usage

`M-x timestamp-at-point` converts:

- seconds or milliseconds in one day, as a duration;
- seconds or milliseconds posix timestamps from 2001 to 2286, as a time.
