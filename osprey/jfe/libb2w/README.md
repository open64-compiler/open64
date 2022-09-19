# OPENCC - CMAKE Builder


## 0. Please do the following change to the original OPEN64 files

### Recommended:

- Using changed version - `n1` (new one)

```
    git clone https://github.com/lugt/open64 open64
    cd open64
    git fetch
    git checkout n1
```    

### Alternative: you may use my changed files (Not Recommended)
- Extract the files from utils/changed.tar.gz into the open64 dir. (outside osprey dir)
- Cheers!

## 1. Set your open64/osprey dir path into global/dir.cmake

You may copy the `dir.cmake.example` to `dir.cmake` and then edit the file


## 2. In the opencc_cmake dir,

create a new folder called `cmake-build-debug` and then compile the project inside `cmake-build-debug` to make convenience for future actions(like deletion or rerun cmake)

```
nginx@opencc_cmake$ mkdir cmake-build-debug
nginx@opencc_cmake$ cd cmake-build-debug
```

Run cmake then

```
nginx@cmake-build-debug$ cmake ..
```

If it ends with no promt of errors, you shall proceed to make.
```
nginx@cmake-build-debug$ make
```

## 3. Partial building
- Should you want to build a part of it, rather than the whole project, just simply
```
 cmake ..  --> cmake ../MODULE_NAME

e.g.  cmake ../irtools


## 4. Miselleneous

- If you would like to rerun cmake, or change from building the whole project to a partial of it,
Please just remove all files in the `cmake-build-debug` dir.
inside cmake-build-debug, run `rm -r *`


- For updating newest version of building scripts

Run `git pull` in opencc_cmake directory.


# Current Modules Available 
libcomutils
libspin
libcmplrs
libiberty
irtools

# On the Edge
whirl2c be be/com be/be be/ipa be/

# A bit later
gcc8.1/fe


# Wiki

Please do checkout wiki to know more