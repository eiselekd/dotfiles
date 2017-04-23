package flagpath;

use Getopt::Long;
use File::Basename;
use File::Path;
use FindBin qw($Bin);
use Cwd;
use Data::Dumper;
use Carp;

# append flag array to filename
sub flag {
    my ($fn,$f) = @_;
    my @f = @$f;
    return $fn, if (!scalar(@f));
    return $fn."{".join(",",sort(@f))."}";
}

# scan filename for flags, return unflagged filename and flags seperately
sub unflag {
    my ($fn) = @_;
    my %h = ();
    $fn =~ /(.*)\{([a-z,]+)\}$/ or $fn =~ /(.*)($)/ or (die("Cannot scan $fn"));
    my $n = $1;
    map { $h{$_}=1} split(",",$2);
    return ($n,[sort(keys(%h))]);
}

# glob filename to retrive flagged filename
sub  findflagged {
    my ($b,$fn) = @_;
    my @g = glob "$b/${fn}*";
    @g = grep { $_ =~ /\/${fn}(?:$|\{[a-z,]+\}$)/ } @g;
    die ("Multiple flagged names $fn in $b") if ((scalar(@g)) > 1);
    return [undef,$fn,[]] if (scalar(@g) == 0);
    return [$g[0],unflag($g[0])];
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


sub test {
    `touch /tmp/gen\{a,b,c\}`;
    my $f = findflagged("/tmp","gen");
    print Dumper($f);
    `touch /tmp/ge`;
    my $f = findflagged("/tmp","geno");
    print Dumper($f);
}

##############################
package flagpath::path;
@ISA = ('flagpath');

sub new {
    my ($c,$p) = @_;
    my @d = flagpath::splitpath($p);
    
    foreach my $d (@d) {
	new flagpath::part(flagpath::part($d));
    }
    
    my $s = {};
    bless $s,$c;
    return $s;
}


##############################
package flagpath::part;
@ISA = ('flagpath');

sub new {
	my ($c) = @_;
	my $s = {};
	bless $s,$c;
	return $s;
}





test();

1;

# Local Variables:                                                                                      # tab-width: 4                                                                                          # cperl-indent-level: 4                                                                                 # End:

