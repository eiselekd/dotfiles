#!/usr/bin/perl
# this script generates a merged tree out of the output of "zfs diff".
# 1. first generate a diff:  zfs diff rpool/root@a rpool/root@b > a.txt
# 2. call zfs_diff.pl with --from, --to  and a.txt:
#    --from and --to should poin to the snapshot directories:
#    zfs_diff.pl --from=<rpool/root mountpoint>/.zfs/snapshots/a --from=<rpool/root mountpoint>/.zfs/snapshots/b a.txt
# 3. The output directory will be created in /tmp/root/...
    
use Getopt::Long;
use File::Basename;
use File::Path;
use FindBin qw($Bin);
use Cwd;
use Data::Dumper;
use Carp;
use Cwd 'abs_path';
use lib "$Bin/../lib";

sub usage { print("usage: $0 <infiles> [--quiet|--verbose]
    --quiet                 : un-verbose
"); exit(1);
}
Getopt::Long::Configure(qw(bundling));
GetOptions(\%OPT,qw{
    quiet|q+
    verbose|v+
    from=s
    to=s
} ,@g_more) or usage(\*STDERR);

$def = $ARGV[0];

sub readfile {
    my ($in) = @_;
    ::usage(\*STDOUT) if (length($in) == 0) ;
    open IN, "$in" or die "Reading \"$in\":".$!;
    local $/ = undef;
    $m = <IN>;
    close IN;
    return $m;
}

sub splitpath {
    my ($fn) = @_;
    my $name = basename($fn);
    my $dir = dirname($fn);
    my @d = ($name);
    while (length($dir) && ($dir ne "/")) {
	my $base = basename($dir);
	$dir  = dirname($dir);
	unshift(@d, $base);
    }
    return @d;
}

$m = readfile($def);

$root = {'n'=>'root','f'=>{},'c'=>{}};
$from = $OPT{'from'} or die("Specify --from=<ori-snapshot>"); #"/.zfs/snapshot/install";
$to   = $OPT{'to'} or die("Specify --to=<patched-snapshot>"); "/.zfs/snapshot/install-minimal";
$tmp  = "/tmp/dirs4";

sub _tagpath {
    my (@d) = @_;
}


sub tagpath {
    my ($flag,$file,@d) = @_;
    print (" $flag:".join("/",@d).($file?("=".$file):"")."\n") if ($OPT{'verbose'});
    my $c = $root;
    foreach my $d (@d) {
	$$c{'c'}{$d} = {'f'=>{},'n'=>$d} if (!exists($$c{'c'}{$d}));
	$c = $$c{'c'}{$d};
	$$c{'f'}{$flag} = 1;
    }
    $$c{'a'} = $file;
}

#
#$m=<<EOF;
#-	/var/lib/dpkg/info/libpam-systemd:amd64.postinst
#M	/usr/share/locale/nb/LC_MESSAGES/libapt-pkg5.0.mo
#M	/var/lib/systemd
#+	/var/lib/systemd/random-seed
#EOF

my @l = split("\n",$m);

foreach my $l (@l) {
    my $fn,$fn1;
    my @d, @d1; my $f,$flag,$b;
    my $gtree = undef; my $action = {};
    if ($l =~ /^M\s*(.*)/) {
	($f,$flag,$b) = ($1,'m',$from);
	$$action{'m'} = {
	    'src' => "$from/$f",
	    'dst' => "$to/$f",
	};
	if (-d "$from/$f") {
	} else {
	    $$action{'diff'} = {
		'src' => "$from/$f",
		'dst' => "$to/$f",
	    };
	}
    } elsif ($l =~ /^\-\s*(.*)/) {
	($f,$flag,$b) = ($1,'-',$from);
	$$action{'link'} = {
	    'src' => "$from/$f",
	};
    } elsif ($l =~ /^\+\s*(.*)/) {
	($f,$flag,$b) = ($1,'+',$to);
	if (! -d "$to/$f") {
	    $$action{'link'} = {
		'src' => "$to/$f",
	    };
	}
    } elsif ($l =~ /^R\s*(.*) -> (.*)/) {
	($f,$flag,$b,$fto) = ($1,'+',$from,$2);
	if (! -d "$to/$f") {
	    $$action{'link'} = {
		'src' => "$to/$fto",
	    };
	}
    } else {
	die("Cannot classify $l\n");
    }
    
    print ("$flag:$f\n test: $b/$f\n") if $OPT{verbose};
    
    $$action{'r'} = "$b/$f";
    tagpath($flag,$action,splitpath($f));
    
}

sub genpath {
    my ($base,$h) = @_; my $dodir = 1;
    my @lfags = sort (keys %{$$h{'f'}});
    my $flags = scalar(@lfags) ? "{".join(",",@lfags)."}"  :"";
    my $n = $base."/".$$h{'n'}.$flags;
    if (exists($$h{'a'}{'r'})) {
	if ($$h{'a'}{'m'}) {
	    my $m = $$h{'a'}{'m'};
	    `ls -lad $$m{'src'} > /tmp/a.attr`;
	    `ls -lad $$m{'dst'} > /tmp/b.attr`;
	    print (" Attr $n.attr\n") if ($OPT{'verbose'});
	    my $cmd = "diff /tmp/a.attr /tmp/b.attr > '$n.attr'";
	    `$cmd`;
	}
	if ($$h{'a'}{'link'}) {
	    my $diff = $$h{'a'}{'link'};
	    if (-e $$diff{'src'}) { 
		my $cmd = "ln -s '".$$diff{'src'}."' '$n'";
		print (" Link $cmd\n") if ($OPT{'verbose'});
		`$cmd`;
	    }
	}
	if ( -d $$h{'a'}{'r'}) {
	    print ("Path $n\n") if ($OPT{'verbose'});
	    mkpath($n) ;
	} else {
	    $dodir = 0;
	    if ($$h{'a'}{'diff'}) {
		my $diff = $$h{'a'}{'diff'};
		my $cmd = "diff -Naur '".$$diff{'src'}."' '".$$diff{'dst'}."' > '$n.diff'";
		`$cmd`;
		print (" Diff $cmd\n") if ($OPT{'verbose'});
		`ls -la $$diff{'src'} > /tmp/a.attr`;
		`ls -la $$diff{'dst'} > /tmp/b.attr`;
		my $cmd = "diff /tmp/a.attr /tmp/b.attr > '$n.attr'";
		`$cmd`;
	    }
	}
    };

    if ($dodir) {
	print ("Path $n\n") if ($OPT{'verbose'});
	mkpath($n) ;
    }
    foreach my $k (keys(%{$$h{'c'}})) {
	genpath($n,$$h{'c'}{$k});
    }
}

#print (Dumper($root));

genpath("/tmp", $root);
