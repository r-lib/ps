# File system information for files

File system information for files

## Usage

``` r
ps_fs_info(paths = "/")
```

## Arguments

- paths:

  A path or a vector of paths. `ps_fs_info()` returns information about
  the file systems of all paths. `path` may contain direcories as well.

## Value

Data frame with file system information for each path in `paths`, one
row per path. Common columns for all operating systems:

- `path`: The input paths, i.e. the `paths` argument.

- `mountpoint`: Directory where the file system is mounted. On Linux
  there is a small chance that it was not possible to look this up, and
  it is `NA_character_`. This is the drive letter or the mount directory
  on Windows, with a trailing `\`.

- `name`: Device name. On Linux there is a small chance that it was not
  possible to look this up, and it is `NA_character_`. On Windows this
  is the volume GUID path of the form `\\?\Volume{GUID}\`.

- `type`: File system type (character). On Linux there is a tiny chance
  that it was not possible to look this up, and it is `NA_character_`.

- `block_size`: File system block size. This is the sector size on
  Windows, in bytes.

- `transfer_block_size`: Pptimal transfer block size. On Linux it is
  currently always the same as `block_size`. This is the cluster size on
  Windows, in bytes.

- `total_data_blocks`: Total data blocks in file system. On Windows this
  is the number of sectors.

- `free_blocks`: Free blocks in file system. On Windows this is the
  number of free sectors.

- `free_blocks_non_superuser`: Free blocks for a non-superuser, which
  might be different on Unix. On Windows this is the number of free
  sectors for the calling user.

- `id`: File system id. This is a raw vector. On Linux it is often all
  zeros. It is always `NULL` on Windows.

- `owner`: User that mounted the file system. On Linux and Windows this
  is currently always `NA_real_`.

- `type_code`: Type of file system, a numeric code. On Windows this this
  is `NA_real_`.

- `subtype_code`: File system subtype (flavor). On Linux and Windows
  this is always `NA_real_`.

The rest of the columns are flags, and they are operating system
dependent.

macOS:

- `RDONLY`: A read-only filesystem.

- `SYNCHRONOUS`: File system is written to synchronously.

- `NOEXEC`: Can't exec from filesystem.

- `NOSUID`: Setuid bits are not honored on this filesystem.

- `NODEV`: Don't interpret special files.

- `UNION`: Union with underlying filesysten.

- `ASYNC`: File system written to asynchronously.

- `EXPORTED`: File system is exported.

- `LOCAL`: File system is stored locally.

- `QUOTA`: Quotas are enabled on this file system.

- `ROOTFS`: This file system is the root of the file system.

- `DOVOLFS`: File system supports volfs.

- `DONTBROWSE`: File system is not appropriate path to user data.

- `UNKNOWNPERMISSIONS`: VFS will ignore ownership information on
  filesystem filesystemtem objects.

- `AUTOMOUNTED`: File system was mounted by automounter.

- `JOURNALED`: File system is journaled.

- `DEFWRITE`: File system should defer writes.

- `MULTILABEL`: MAC support for individual labels.

- `CPROTECT`: File system supports per-file encrypted data protection.

Linux:

- `MANDLOCK`: Mandatory locking is permitted on the filesystem (see
  `fcntl(2)`).

- `NOATIME`: Do not update access times; see `mount(2)`.

- `NODEV`: Disallow access to device special files on this filesystem.

- `NODIRATIME`: Do not update directory access times; see mount(2).

- `NOEXEC`: Execution of programs is disallowed on this filesystem.

- `NOSUID`: The set-user-ID and set-group-ID bits are ignored by
  `exec(3)` for executable files on this filesystem

- `RDONLY`: This filesystem is mounted read-only.

- `RELATIME`: Update atime relative to mtime/ctime; see `mount(2)`.

- `SYNCHRONOUS`: Writes are synched to the filesystem immediately (see
  the description of `O_SYNC` in \`open(2)“).

- `NOSYMFOLLOW`: Symbolic links are not followed when resolving paths;
  see \`mount(2)“.

Windows:

- `CASE_SENSITIVE_SEARCH`: Supports case-sensitive file names.

- `CASE_PRESERVED_NAMES`: Supports preserved case of file names when it
  places a name on disk.

- `UNICODE_ON_DISK`: Supports Unicode in file names as they appear on
  disk.

- `PERSISTENT_ACLS`: Preserves and enforces access control lists (ACL).
  For example, the NTFS file system preserves and enforces ACLs, and the
  FAT file system does not.

- `FILE_COMPRESSION`: Supports file-based compression.

- `VOLUME_QUOTAS`: Supports disk quotas.

- `SUPPORTS_SPARSE_FILES`: Supports sparse files.

- `SUPPORTS_REPARSE_POINTS`: Supports reparse points.

- `SUPPORTS_REMOTE_STORAGE`: Supports remote storage.

- `RETURNS_CLEANUP_RESULT_INFO`: On a successful cleanup operation, the
  file system returns information that describes additional actions
  taken during cleanup, such as deleting the file. File system filters
  can examine this information in their post-cleanup callback.

- `SUPPORTS_POSIX_UNLINK_RENAME`: Supports POSIX-style delete and rename
  operations.

- `VOLUME_IS_COMPRESSED`: It is a compressed volume, for example, a
  DoubleSpace volume.

- `SUPPORTS_OBJECT_IDS`: Supports object identifiers.

- `SUPPORTS_ENCRYPTION`: Supports the Encrypted File System (EFS).

- `NAMED_STREAMS`: Supports named streams.

- `READ_ONLY_VOLUME`: It is read-only.

- `SEQUENTIAL_WRITE_ONCE`: Supports a single sequential write.

- `SUPPORTS_TRANSACTIONS`: Supports transactions.

- `SUPPORTS_HARD_LINKS`: The volume supports hard links.

- `SUPPORTS_EXTENDED_ATTRIBUTES`: Supports extended attributes.

- `SUPPORTS_OPEN_BY_FILE_ID`: Supports open by FileID.

- `SUPPORTS_USN_JOURNAL`: Supports update sequence number (USN)
  journals.

- `SUPPORTS_INTEGRITY_STREAMS`: Supports integrity streams.

- `SUPPORTS_BLOCK_REFCOUNTING`: The volume supports sharing logical
  clusters between files on the same volume.

- `SUPPORTS_SPARSE_VDL`: The file system tracks whether each cluster of
  a file contains valid data (either from explicit file writes or
  automatic zeros) or invalid data (has not yet been written to or
  zeroed).

- `DAX_VOLUME`: The volume is a direct access (DAX) volume.

- `SUPPORTS_GHOSTING`: Supports ghosting.

## Examples

``` r
ps_fs_info(c("/", "~", "."))
#> # A data frame: 3 × 25
#>   path  mountpoint name      type  block_size transfer_block_size
#>   <chr> <chr>      <chr>     <chr>      <dbl>               <dbl>
#> 1 /     /          /dev/root ext4        4096                4096
#> 2 ~     /          /dev/root ext4        4096                4096
#> 3 .     /          /dev/root ext4        4096                4096
#> # ℹ 19 more variables: total_data_blocks <dbl>, free_blocks <dbl>,
#> #   free_blocks_non_superuser <dbl>, total_nodes <dbl>,
#> #   free_nodes <dbl>, id <list>, owner <dbl>, type_code <dbl>,
#> #   subtype_code <dbl>, MANDLOCK <lgl>, NOATIME <lgl>, NODEV <lgl>,
#> #   NODIRATIME <lgl>, NOEXEC <lgl>, NOSUID <lgl>, RDONLY <lgl>,
#> #   RELATIME <lgl>, SYNCHRONOUS <lgl>, NOSYMFOLLOW <lgl>
```
