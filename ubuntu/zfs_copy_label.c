/*
  zfs_copy_lable -b <backupfile-temp> <dev> : search backup label
  zfs_copy_lable -B <backupfile-temp> <dev> : restore label from backup
  zfs_copy_lable <dev-from> <dev-to>        : reconstruct label of raidz1 device from one online <dev-from>

  Rational: raintz1 array with 3 disks.
  One disk's GPT was distroyed and repartitioning it destroyed also
  the labels. Take a label from one of the functional disk and reconstruct
  the faulty disk label with it.

  This util is specific to a given setup and not to be used by anyone else
*/

#define _GNU_SOURCE
#define _REENTRANT
#define _FILE_OFFSET_BITS 64
#define _LARGEFILE64_SOURCE
#define HAVE_LARGE_STACKS 1

#include <dirent.h>
#include <errno.h>
#include <libintl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <stddef.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/spa.h>
#include <sys/vdev.h>
#include <sys/vdev_impl.h>
#include <sys/sha2.h>

struct rescue_disc {
    uint64_t size;
    struct {
	vdev_label_t vl;
	nvlist_t *config;
    } label[4];
};

struct rescue_disc disk[3];

/* disk 0 */
#define DISK0_LABEL01 "disk0-start.bin"
#define DISK0_LABEL23 "disk0-end.bin"

/* disk 1 */
#define DISK1_LABEL01 "disk1-start.bin"
#define DISK1_LABEL23 "disk1-end.bin"

/* disk 2 */
#define DISK2_LABEL01 "disk2-start.bin"
#define DISK2_LABEL23 "disk2-end.bin"

void load_label_file(vdev_label_t *vl, char *fn, int idx) {
    int fd; uint64_t r;
    if ((fd = open(fn, O_RDONLY)) != -1) {
	if ((r = pread64(fd, (void*)vl, sizeof (vdev_label_t), sizeof(vdev_label_t)*idx)) != sizeof (vdev_label_t)) {
	    printf("Not enough data in '%s', try read %lu got %lu\n", fn, sizeof (vdev_label_t), r);
	}
	close(fd);
    } else
	printf("Cannot open file %s\n", fn);
}

void load_disk_file() {

    load_label_file(&disk[1].label[0].vl, DISK1_LABEL01, 0);
    load_label_file(&disk[1].label[1].vl, DISK1_LABEL01, 1);
    load_label_file(&disk[1].label[2].vl, DISK1_LABEL23, 0);
    load_label_file(&disk[1].label[3].vl, DISK1_LABEL23, 1);

    load_label_file(&disk[2].label[0].vl, DISK2_LABEL01, 0);
    load_label_file(&disk[2].label[1].vl, DISK2_LABEL01, 1);
    load_label_file(&disk[2].label[2].vl, DISK2_LABEL23, 0);
    load_label_file(&disk[2].label[3].vl, DISK2_LABEL23, 1);

    /* copy disk 1's config for disk 0 */
    for (int i = 0; i < 4; i++) {
	disk[0].label[i] = disk[1].label[i];
    }
}

static void
setsha256(uint64_t offset, uint64_t size, void *buf)
{
    zio_eck_t *zbt, zbt_orig;
    zio_cksum_t zc, tmp;

    zbt = (zio_eck_t *)((char *)buf + size) - 1;
    zbt_orig = *zbt;

    ZIO_SET_CHECKSUM(&zbt->zec_cksum, offset, 0, 0, 0);

    SHA2_CTX ctx;
    SHA2Init(SHA256, &ctx);
    SHA2Update(&ctx, buf, size);
    SHA2Final(&tmp, &ctx);

    zc.zc_word[0] = BE_64(tmp.zc_word[0]);
    zc.zc_word[1] = BE_64(tmp.zc_word[1]);
    zc.zc_word[2] = BE_64(tmp.zc_word[2]);
    zc.zc_word[3] = BE_64(tmp.zc_word[3]);

    zbt->zec_cksum = zc;
}

int main(int argc, char**argv)
{

    int c;
    int fd; int verbose = 0;

    opterr = 0;

    while ((c = getopt (argc, argv, "vb:")) != -1)
	switch (c)
	{
	case 'v':
	    verbose = 1;
	    break;
	case 'b':
	    break;
	default:
	    abort ();
	}

    argv += optind;
    argc -= optind;

    load_disk_file();

    /* load disk[1,2] label */
    for (int j = 0; j < 4; j++) {
	nvlist_t *config;
	char *buf = disk[0].label[j].vl.vl_vdev_phys.vp_nvlist;
	size_t buflen = sizeof (disk[0].label[j].vl.vl_vdev_phys.vp_nvlist);

	 VERIFY(nvlist_unpack(buf, buflen, &config, 0) == 0);

	(void) nvlist_remove_all(config, ZPOOL_CONFIG_GUID);
	VERIFY(nvlist_add_uint64(config, ZPOOL_CONFIG_GUID,
				 9231358437407247535UL) == 0);

	VERIFY(nvlist_pack(config, &buf, &buflen, NV_ENCODE_XDR, 0) == 0);

	setsha256(offsetof(vdev_label_t, vl_vdev_phys),
		  VDEV_PHYS_SIZE, &disk[0].label[j].vl.vl_vdev_phys);

    }

    FILE *f;
    if ((f = fopen(DISK0_LABEL01, "wb"))) {
	fwrite(&disk[0].label[0].vl, 1, sizeof(vdev_label_t), f);
	fwrite(&disk[0].label[1].vl, 1, sizeof(vdev_label_t), f);
	fclose(f);
    }
    if ((f = fopen(DISK0_LABEL23, "wb"))) {
	fwrite(&disk[0].label[2].vl, 1, sizeof(vdev_label_t), f);
	fwrite(&disk[0].label[3].vl, 1, sizeof(vdev_label_t), f);
	fclose(f);
    }

}
