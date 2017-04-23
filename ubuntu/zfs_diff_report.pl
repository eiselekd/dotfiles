#!/usr/bin/perl
    
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
} ,@g_more) or usage(\*STDERR);

$def = $ARGV[0];

sub ltrim { my $s = shift; $s =~ s/^\s+//;       return $s };
sub rtrim { my $s = shift; $s =~ s/\s+$//;       return $s };
sub  trim { my $s = shift; $s =~ s/^\s+|\s+$//g; return $s };

sub readfile {
    my ($in) = @_;
    ::usage(\*STDOUT) if (length($in) == 0) ;
    open IN, "$in" or die "Reading \"$in\":".$!;
    local $/ = undef;
    $m = <IN>;
    close IN;
    return $m;
}

sub writefile {
  my ($out,$re,$temp) = @_;
  my $dir = dirname($out);
  if ($dir) {
    mkpath($dir);
  }
    open OUT, ">$out" or die ($out.$!);
    print OUT ($re);
    close OUT;
}

sub splitpath {
    my ($fn) = @_;
    my $name = basename($fn);
    my $dir = dirname($fn);
    my @d = ();
    while (length($dir) && ($dir ne "/")) {
	my $base = basename($dir);
	$dir  = dirname($dir);
	unshift(@d, $base);
    }
    return ($name, @d);
}

sub testdir {
    my ($b, $fn, @d) = @_;
    my $p = join("/", ($b, @d, $fn));
    if (-d $p) {
	return 1;
    }
    return 0;
}

sub gendir {
    my ($b, @d) = @_;
    my $p = join("/", ($b, @d));
    mkpath $p;
}

sub extractflags {
    my ($fn) = @_;
    my %h = ();
    $fn = /(.*)\{([armM,]+)\}/;
    my $n = $1;
    map { $h{$_}} split(",",$2);
    return ($n,sort(keys(%h)));
}



sub touchflags {
    my ($flags,$b,$d) = @_;
    my @d = @$d;
    foreach my $c (@d) {
	my @g = glob($b."/".$c.'{*');

	
	
	$b = $b."/".$c;
    }
    
    if (defined($gtree)) {
	my $d = dirname($gtree);
	if (length($d)) {
	    mkpath $d;
	}
	`touch '$gtree'`;
    }
}

sub touchmkpath {
    my ($gtree) = @_;
    if (defined($gtree)) {
	my $d = dirname($gtree);
	if (length($d)) {
	    mkpath $d;
	}
	`touch '$gtree'`;
    }
}

$m = readfile($def);

$from = "/.zfs/snapshot/install";
$to   = "/.zfs/snapshot/install-minimal";
$tmp  = "/tmp/dirs4";

foreach my $l (split("\n",$m)) {
    my $fn,$fn1;
    my @d, @d1;
    my $gtree = undef;
    if ($l =~ /^M\s*(.*)/) {
	print("modify: $1\n") if $OPT{'verbose'};
	($fn,@d) = splitpath($1);
	if (testdir($from,$fn, @d )) {
	    gendir($tmp,@d,$fn);
	} else {
	    gendir($tmp.".modified",@d);
	    my $n0 = join("/", ($from, @d, $fn));
	    my $n1 = join("/", ($to, @d, $fn));
	    my $n2 = join("/", ($tmp.".modified", @d, $fn.".diff"));
	    my $n2_a = join("/", ($tmp.".modified", @d, "_attr.".$fn.".diff"));
    	    `ls -la '$n0' '$n1' > '$n2_a'; diff -Naur '$n0' '$n1' > '$n2'`;
	    touchmkpath(join("/", ($tmp, @d, $fn)));
	}
    } elsif ($l =~ /^\-\s*(.*)/) {
	print("remove: $1\n") if $OPT{'verbose'};
	($fn,@d) = splitpath($1);
	gendir($tmp,@d,$fn) if (testdir($from,$fn,@d));
	touchmkpath(join("/", ($tmp.".remove", @d, $fn.".__remove")));
    } elsif ($l =~ /^\+\s*(.*)/) {
	print("added : $1\n") if $OPT{'verbose'};
	($fn,@d) = splitpath($1);
	gendir($tmp,@d,$fn) if (testdir($to,$fn,@d));
	$gtree = join("/", ($tmp,@d, $fn));
	touchmkpath(join("/", ($tmp.".added", @d, $fn.".__added")));
    } elsif ($l =~ /^R\s*(.*) -> (.*)/) {
	print("rename: $1\n") if $OPT{'verbose'};
	($fn,@d) = splitpath($1);
	($fn1,@d1) = splitpath($2);
	gendir($tmp,@d,$fn) if (testdir($from,$fn,@d));
	$gtree = join("/", ($tmp, @d, $fn));

	my $n = join("/", ($tmp.".rename", @d, $fn.".__rename"));
	touchmkpath($n);
	
	my $c = "echo '".join("/", ($tmp, @d1, $fn1))."' > '$n'";
	`$c`;
    } else {
	die("Cannot classify $l\n");
    }
    
}

