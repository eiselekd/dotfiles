import sys

def _map_alloc(io_offset, io_size, unit_shift, dcols, nparity):

    # The starting RAIDZ (parent) vdev sector of the block.
    b = io_offset >> unit_shift
    # The zio's size in units of the vdev's minimum sector size.
    s = io_size >> unit_shift
    # The first column for this stripe.
    f = b % dcols
    # The starting byte offset on each child vdev.
    o = (b // dcols) << unit_shift

    print("[+] : 0x%x:0x%x ashift:%d,%d,%d" %(io_offset, io_size, unit_shift, dcols, nparity))

    # "Quotient": The number of data sectors for this stripe on all but
    # the "big column" child vdevs that also contain "remainder" data.
    q = s // (dcols - nparity)

    # "Remainder": The number of partial stripe data sectors in this I/O.
    # This will add a sector to some, but not all, child vdevs.
    r = s - q * (dcols - nparity)

    # The number of "big columns" - those which contain remainder data.
    bc = (r + nparity) if r else 0

    # The total number of data and parity sectors associated with
    # this I/O.
    tot = s + nparity * (q + (1 if r else 0))

    # acols: The columns that will be accessed.
    # scols: The columns that will be accessed or skipped.
    if q == 0:
        # Our I/O request doesn't span all child vdevs.
        acols = bc
        scols = min(dcols, roundup(bc, nparity + 1))
    else:
        acols = dcols
        scols = dcols

    rm_skipstart = bc
    rm_firstdatacol = nparity
    rm_cols = []

    for c in range(scols):
        col = f + c
        coff = o
        rm_col = {"rc_size" : 0}
        if col >= dcols:
            col -= dcols
            coff += (1 << unit_shift)
        rm_col = {"rc_devidx": col, "rc_offset": coff}

        if c >= acols:
            rm_col["rc_size"] = 0
        elif c < bc:
            rm_col["rc_size"] = (q + 1) << unit_shift
        else:
            rm_col["rc_size"] = q << unit_shift
        if rm_col["rc_size"] > 0:
            rm_cols.append(rm_col)

    if (rm_firstdatacol == 1) and (io_offset & (1 << 20)):
        devidx = rm_cols[0]["rc_devidx"]
        o = rm_cols[0]["rc_offset"]
        rm_cols[0]["rc_devidx"] = rm_cols[1]["rc_devidx"]
        rm_cols[0]["rc_offset"] = rm_cols[1]["rc_offset"]
        rm_cols[1]["rc_devidx"] = devidx
        rm_cols[1]["rc_offset"] = o
        if rm_skipstart == 0:
            rm_skipstart = 1

    return rm_cols, rm_firstdatacol, rm_skipstart

def main():
    ashift=12
    dcols=3
    nparity=1
    if len(sys.argv) <= 1:
        print("Usage: off:size [ashift,dcols,nparity]"); exit(1);
    a = sys.argv[1].split(":");
    offset=int(a[0],16);
    size=int(a[1],16);
    if len(sys.argv) > 2:
        ashift = int(sys.argv[2]);
    if len(sys.argv) > 3:
        dcols = int(sys.argv[3]);
    if len(sys.argv) > 4:
        nparity = int(sys.argv[4]);

    rm_cols, rm_firstdatacol, rm_skipstart = _map_alloc(offset, size, ashift, dcols, nparity)

    print("cols = %d, firstdatacol = %d" %(len(rm_cols), rm_firstdatacol));
    for i in range(len(rm_cols)):
        print("%d:%lx:%lx" %(rm_cols[i]['rc_devidx'], rm_cols[i]['rc_offset'], rm_cols[i]['rc_size']))



if __name__ == "__main__":
    main()
