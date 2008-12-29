# Copyrights 2008 by Mark Overmeer.
#  For other contributors see ChangeLog.
# See the manual pages for details on the licensing terms.
# Pod stripped from pm file by OODoc 1.05.
use warnings;
use strict;

package XML::Compile::Cache;
use vars '$VERSION';
$VERSION = '0.14';

use base 'XML::Compile::Schema';

use Log::Report 'xml-compile-cache', syntax => 'SHORT';

use XML::Compile::Util   qw/pack_type unpack_type/;
use List::Util           qw/first/;


sub init($)
{   my ($self, $args) = @_;
    $self->SUPER::init($args);

    $self->{XCC_opts}   = delete $args->{opts_rw}      || [];
    $self->{XCC_ropts}  = delete $args->{opts_readers} || [];
    $self->{XCC_wopts}  = delete $args->{opts_writers} || [];
    $self->{XCC_undecl} = delete $args->{allow_undeclared} || 0;

    $self->{XCC_rcode}  = {};
    $self->{XCC_wcode}  = {};
    $self->{XCC_dropts} = {};
    $self->{XCC_dwopts} = {};
    $self->{XCC_uropts} = {};
    $self->{XCC_uwopts} = {};

    $self->{XCC_readers} = {};
    $self->{XCC_writers} = {};

    my $p = $self->{XCC_namespaces}
          = $self->_namespaceTable(delete $args->{prefixes});
    my %a = map { ($_->{prefix} => $_) } values %$p;
    $self->{XCC_prefixes} = \%a;

    if(my $anyelem = $args->{any_element})
    {   if($anyelem eq 'ATTEMPT')
        {   push @{$self->{XCC_ropts}}
              , any_element => sub {$self->_convertAnyElementReader(@_)};
        }
    }

    $self;
}

#----------------------


sub prefixes(@)
{   my $self = shift;
    my ($p, $a) = @$self{ qw/XCC_namespaces XCC_prefixes/ };
    while(@_)
    {   my ($prefix, $ns) = (shift, shift);
        $p->{$ns} ||= { uri => $ns, prefix => $prefix, used => 0 };

        if(my $def = $a->{$prefix})
        {   if($def->{uri} ne $ns)
            {   error __x"prefix {prefix} already refers to {uri}, cannot use it for {ns}"
                  , prefix => $prefix, uri => $def->{uri}, ns => $ns;
            }
        }
        else
        {   $a->{$prefix} = $p->{$ns};
        }
    }
    $p;
}


sub prefix($) { $_[0]->{XCC_prefixes}{$_[1]} }


sub prefixFor($)
{   my $def = $_[0]->{XCC_namespaces}{$_[1]} or return ();
    $def->{used}++;
    $def->{prefix};
}


sub allowUndeclared(;$)
{   my $self = shift;
    @_ ? ($self->{XCC_undecl} = shift) : $self->{XCC_undecl};
}

#----------------------


sub compileAll(;$$)
{   my ($self, $need, $usens) = @_;
    my ($need_r, $need_w) = $self->_need($need);

    if($need_r)
    {   while(my($type, $opts) = each %{$self->{XCC_dropts}})
        {   if(defined $usens)
            {   my ($myns, $local) = unpack_type $type;
                next if $usens eq $myns;
            }
            $self->{XCC_rcode}{$type} ||= $self->_createReader($type, @$opts);
        }
    }

    if($need_w)
    {   while(my($type, $opts) = each %{$self->{XCC_dwopts}})
        {   if(defined $usens)
            {   my ($myns, $local) = unpack_type $type;
                next if $usens eq $myns;
            }
            $self->{XCC_wcode}{$type} ||= $self->_createWriter($type, @$opts);
        }
    }
}


sub reader($@)
{   my ($self, $name) = (shift, shift);
    my $type    = $self->findName($name);
    my $readers = $self->{XCC_readers};

    if(exists $self->{XCC_dropts}{$type})
    {   warn __x"ignoring options to pre-declared reader {name}"
          , name => $name if @_;

        return $readers->{$type}
            if $readers->{$type};
    }
    elsif($self->allowUndeclared)
    {   if(my $ur = $self->{XCC_uropts}{$type})
        {   my $differs = @$ur != @_;
            unless($differs)
            { for(my $i=0; $i<@$ur; $i++) {$differs++ if $ur->[$i] ne $_[$i]} } 

            # do not use cached version when options differ
            return $self->_createReader($type, @_)
                if $differs;
        }
        else
        {   $self->{XCC_uropts}{$type} = \@_;
        }
    }
    elsif(exists $self->{XCC_dwopts}{$type})
         { error __x"type {name} is only declared as writer", name => $name }
    else { error __x"type {name} is not declared", name => $name }

    $readers->{$type} ||= $self->_createReader($type, @_);
}

sub _createReader($@)
{   my ($self, $type) = (shift, shift);
    trace "create reader for $type";

    $self->compile(READER => $type,
      , $self->mergeCompileOptions($self->{XCC_opts}
          , $self->{XCC_ropts}, $self->{XCC_dropts}{$type}, \@_)
      );
}


sub writer($)
{   my ($self, $name) = (shift, shift);
    my $type = $self->findName($name);
    my $writers = $self->{XCC_writers};

    if(exists $self->{XCC_dwopts}{$type})
    {   warn __x"ignoring options to pre-declared writer {name}"
          , name => $name if @_;

        return $writers->{$type}
            if $writers->{$type};
    }
    elsif($self->{XCC_undecl})
    {   if(my $ur = $self->{XCC_uwopts}{$type})
        {   my $differs = @$ur != @_;
            unless($differs)
            { for(my $i=0; $i<@$ur; $i++) {$differs++ if $ur->[$i] ne $_[$i]} }

            # do not use cached version when options differ
            return $self->_createWriter($type, @_)
                if $differs;
        }
        else
        {   $self->{XCC_uwopts}{$type} = \@_;
        }
    }
    elsif(exists $self->{XCC_dropts}{$type})
    {   error __x"type {name} is only declared as reader", name => $name;
    }
    else
    {   error __x"type {name} is not declared", name => $name;
    }

    $writers->{$type} ||= $self->_createWriter($type, @_);
}

sub _createWriter($)
{   my ($self, $type) = @_;
    
    trace "create writer for $type";
    $self->compile(WRITER => $type, $self->mergeCompileOptions
       ($self->{XCC_opts}, $self->{XCC_wopts}, $self->{XCC_dwopts}{$type}, \@_)
      );
}

# Create a list with options for X::C::Schema::compile(), from a list of ARRAYs
# and HASHES with options.  The later options overrule the older, but in some
# cases, the new values are added.  This method knows how some of the options
# of ::compile() behave.  [last update X::C v0.98]

sub mergeCompileOptions(@)
{   my $self = shift;
    my %p    = %{$self->{XCC_namespaces}};
    my %opts = (prefixes => \%p, hooks => [], typemap => {});

    # flatten list of parameters
    my @take = map {!defined $_ ? () : ref $_ eq 'ARRAY' ? @$_ : %$_ } @_;

    while(@take)
    {   my ($opt, $val) = (shift @take, shift @take);
        if($opt eq 'hook')
        {   ($opt, $val) = (hooks => (ref $val eq 'ARRAY' ? {@$val} : $val));
        }

        if($opt eq 'prefixes')
        {   my %t = $self->_namespaceTable($val, 1, 0);  # expand
            @p{keys %t} = values %t;   # overwrite old def if exists
        }
        elsif($opt eq 'hooks')
        {   my @hooks = ref $val eq 'ARRAY' ? @$val : defined $val ? $val : ();
            foreach my $hook (grep {$_->{type}} @hooks)   # rewrite prefixed names
            {   my $types = $hook->{type};
                $hook->{type} =
                  [ map {ref $_ eq 'Regexp' ? $_ : $self->findName($_)}
                       ref $types eq 'ARRAY' ? @$types : $types ];
            }
            unshift @{$opts{hooks}}, @hooks;
        }
        elsif($opt eq 'typemap')
        {   $val ||= {};
            @{$opts{typemap}}{keys %$val} = values %$val;
        }
        elsif($opt eq 'key_rewrite')
        {   unshift @{$opts{key_rewrite}}, ref $val eq 'ARRAY' ? @$val : $val;
        }
        else
        {   $opts{$opt} = $val;
        }
    }

    %opts;
}

sub _need($)
{    $_[1] eq 'READER' ? (1,0)
   : $_[1] eq 'WRITER' ? (0,1)
   : $_[1] eq 'RW'     ? (1,1)
   : error __x"use READER, WRITER or RW, not {dir}", dir => $_[1];
}

# support 
sub compile($$@)
{   my ($self, $action, $type, @args) = @_;
    $self->SUPER::compile($action, $self->findName($type), @args);
}

#----------------------

sub declare($$@)
{   my ($self, $need, $names, @opts) = @_;
    my $opts = @opts==1 ? shift @opts : \@opts;
    $opts = [ %$opts ] if ref $opts eq 'HASH';

    my ($need_r, $need_w) = $self->_need($need);

    foreach my $name (ref $names eq 'ARRAY' ? @$names : $names)
    {   my $type = $self->findName($name);
        trace "declare $type";

        if($need_r)
        {   defined $self->{XCC_dropts}{$type}
               and warning __x"reader {name} declared again", name => $name;
            $self->{XCC_dropts}{$type} = $opts;
        }

        if($need_w)
        {   defined $self->{XCC_dwopts}{$type}
               and warning __x"writer {name} declared again", name => $name;
            $self->{XCC_dwopts}{$type} = $opts;
        }
    }

    $self;
}


sub findName($)
{   my ($self, $name) = @_;
    defined $name
        or panic "findName called without name";

    return $name if $name =~ m/^\{/;

    my ($prefix,$local) = $name =~ m/^([\w-]*)\:(\S*)$/ ? ($1,$2) : ('',$name);
    my $def = $self->{XCC_prefixes}{$prefix};
    unless($def)
    {   return $name if $prefix eq '';   # namespace-less
        trace __x"known prefixes: {prefixes}"
          , prefixes => [ sort keys %{$self->{XCC_prefixes}} ];
        error __x"unknown name prefix for `{name}'", name => $name;
    }

    length $local ? pack_type($def->{uri}, $local) : $def->{uri};
}


sub printIndex(@)
{   my $self = shift;
    my $fh   = @_ % 2 ? shift : select;
    my %args = @_;
    my $decl = exists $args{show_declared} ? delete $args{show_declared} : 1;

    return $self->SUPER::printIndex($fh, %args)
        unless $decl;

    my $output = '';
    open my($out), '>', \$output;

    $self->SUPER::printIndex($out, %args);

    close $out;
    my @output = split /(?<=\n)/, $output;
    my $ns     = '';
    foreach (@output)
    {   $ns = $1 if m/^namespace\:\s+(\S+)/;
        my $local = m/^\s+(\S+)\s*$/ ? $1 : next;
        my $type  = pack_type $ns, $local;

        substr($_, 1, 1)
          = $self->{XCC_readers}{$type} ? 'R'
          : $self->{XCC_dropts}{$type}  ? 'r' : ' ';

        substr($_, 2, 1)
          = $self->{XCC_writers}{$type} ? 'W'
          : $self->{XCC_dwopts}{$type}  ? 'w' : ' ';
    }
    print $fh @output;
}

#---------------
# Convert ANY elements and attributes

sub _convertAnyElementReader(@)
{   my ($self, $type, $nodes, $path, $read, $args) = @_;

    my $reader  = try { $self->reader($type) };
    !$@ && $reader or return ($type => $nodes);

    my @nodes   = ref $nodes eq 'ARRAY' ? @$nodes : $nodes;
    my @convert = map {$reader->($_)} @nodes;

    my $key     = $read->keyRewrite($type);
    ($key => @convert==1 ? $convert[0] : \@convert);
}

1;
