package Nenm::Utils;

=pod

=head1 NAME

Nagios Enterprise Network Monitoring Utils

=head1 SYNOPSIS

A collection of routines that make multi-threshold parsing and checking
easier, these routines let us check warning and critical thresholds that
are in the format 'metric,<op>,value:metric2,<op>,value2', e.g. 
'nice,gt,90:user,lt,5'.

Valid operators are  gt (>), lt (<), gte (>=), ne (!=), eq (==), and lte (<=).  
Valid metrics are passed into the parse and check functions as a hash ref 
where the keys are the metrics and the values are hashes with at least a key 
called 'value' that contains the value associated with the metric.

=cut

$Nenm::Utils::DEBUG = 0;

use strict;
use Nagios::Plugin;    #  For error levels

=pod

=head2 parse_multi_threshold($conditions_ref, $valid_metrics);

Parse warning and critical condition strings passed in by the user; returns
a reference to an array of arrays containing thresholds that the function
check_multi_thresholds can check and an array of error messages from errors
that occurred during parsing .. if no errors occur, the size of the error
array will be 0.

Example:

my %metrics = (
    'idle' => {'value' => 0},
    'nice' => {'value' => 0}
);

my ($wthr, $werrs) = Utils::parse_multi_threshold($plugin->opts->warning,
                                                  \%metrics);

=cut

sub parse_multi_threshold {

    my $threshold_conditions = shift
      || die "Missing condition string to parse!";

    my $valid_metrics = shift || die "Missing hash ref of valid metrics";

    my @errors;
    my @thresholds;

    # throw an error in case the metrics has 'default' entry defined.
    # default is a reserved key word for the thresholds.

    my @all_metric_names = keys %$valid_metrics;
    foreach my $a_name (@all_metric_names) {
        if ( $a_name eq "default" ) {

            die "defalut can not be a metric";
        }

    }

   # adding default to the metrics so that thresholds with default will not fail
    my %hello;
    $valid_metrics->{"default"} = \%hello;

    my @conditions = split( ':', $threshold_conditions );

    #  catch overlapping thresholds and throw an error if any exist; we
    #  define overlapping thresholds as two or more conditions where the
    #  metric and operator are the same, e.g.
    #
    #  -w /var,gt,10:/,gt,30:/var,gt,5
    #
    #  in this case we have two thresholds for /var with overlap for the
    #  -w switch.

    my %overlaps      = ();
    my $default_found = 0;
  CONDITION:
    for my $condition (@conditions) {

        my $has_error = 0;

        my ( $metric, $op, $value, @extra ) = split( ',', $condition );

        if ( scalar(@extra) > 0 ) {
            push( @errors,
                    qq{Condition has too many parts!  Should have }
                  . qq{metric,op,value but instead has $condition} );
            next CONDITION;
        }

        if ( !defined($metric) ) {
            push( @errors, "$condition missing metric to check!" );
        }

        if ( !defined($op) ) {
            push( @errors, "$condition missing operator to use for check!" );
        }

        if ( !defined($value) ) {
            push( @errors, "$condition missing value to check!" );
        }

        if ( !exists $valid_metrics->{$metric} ) {
            my $msg = "$metric is not a valid metric, valid metrics " . "are "
              . join( ', ', sort keys %$valid_metrics );
            push( @errors, $msg );
        }

        my $valid_ops = 'lt|gt|gte|ne|eq|lte|re|nre|seq|sne';

        my $real_op = '';

        $op = lc($op);

        if ( $op eq 'lt' ) {
            $real_op = '<';
        }
        elsif ( $op eq 'gt' ) {
            $real_op = '>';
        }
        elsif ( $op eq 'gte' ) {
            $real_op = '>=';
        }
        elsif ( $op eq 'lte' ) {
            $real_op = '<=';
        }
        elsif ( $op eq 'ne' ) {
            $real_op = '!=';
        }
        elsif ( $op eq 'eq' ) {
            $real_op = '==';
        }
        elsif ( $op eq 'seq' ) {
            $metric  = "q{$metric}";
            $real_op = 'eq';
        }
        elsif ( $op eq 'sne' ) {
            $metric  = "q{$metric}";
            $real_op = 'ne';
        }
        elsif ( $op eq 're' ) {
            $metric  = "q{$metric}";
            $real_op = '=~';
            $value   = "/$value/";
        }
        elsif ( $op eq 'nre' ) {
            $metric  = "q{$metric}";
            $value   = "/$value/";
            $real_op = '!~';
        }
        else {
            my $msg = "$op is not a valid operator, valid operators "
              . "are $valid_ops";
            push( @errors, $msg );
        }

        # No undefined value warnings from perl / ePN
        $overlaps{$metric} = {} if !exists $overlaps{$metric};

        if ( !exists $overlaps{$metric}->{$op} ) {

            #  Store count of metric/op pairs.  Index 0 is the
            #  count, index 1 is an array of the conditions that
            #  have the metric and operator.  We store all metric/op
            #  pairs because if there are any overlaps we want to show
            #  the end user all conditions that overlapped, not just
            #  conditions 2 .. N.

            $overlaps{$metric}->{$op} = [ 0, [] ];
            push( @{ $overlaps{$metric}->{$op}->[1] }, $condition );

        }
        else {

            #  Hash exists, means this is the 2nd+ time we have
            #  seen this metric/op pair .. an overlap!

            debug("Found overlapping thresholds for $metric and $op");
            $overlaps{$metric}->{$op}->[0]++;

            push( @{ $overlaps{$metric}->{$op}->[1] }, $condition );

        }

        if ( scalar(@errors) == 0 ) {
            debug(  qq{parse_multi_threshold: adding $metric $real_op }
                  . qq{($op) $value} );
            push( @thresholds, [ $metric, $real_op, $value ] );
        }

        if ( $metric eq "default" ) {
            $default_found = 1;
        }
    }

    for my $overlap_metric ( keys %overlaps ) {

        my $om_ref = $overlaps{$overlap_metric};

        for my $overlap_op ( keys %{$om_ref} ) {

            my $count = $om_ref->{$overlap_op}->[0];

            #  We have overlapping conditions, push a human
            #  readable error message on @errors for the end
            #  user.

            if ( $count > 0 ) {
                $count++;
                my @conditions = @{ $om_ref->{$overlap_op}->[1] };
                push( @errors,
                    qq{$count overlapping thresholds found: }
                      . join( ' ', @conditions ) );
            }

        }
    }
    delete $valid_metrics->{"default"};
    return ( \@thresholds, \@errors, $default_found );

}

=pod

=head2 check_multi_thresholds($metrics, $warning_ref, $critical_ref, $type);

Checks all thresholds in $warning_ref and $critical_ref arrays (arrays
returned by parse_multi_thresholdc calls) and returns a hash of results
with the following keys:
* warning = reference to array of warning messages
* critical = reference to array of critical messages
* ok = reference to array of ok messages
* perfdata = string of perfdata, ready for output

$type is the threshold value type (%, K, M, B) and is added to perfdata
output to indicate the type of number in perfdata output.  Use any
valid perfdata symbol that applies to your data.

Each key in metrics have a value that is a hash reference where there is
at least the key 'value' holding the real value for the metric.

Example:

my %metrics = (
    'idle' => {'value' => 80},
    'nice' => {'value' => 55}
);

my $results = 
    Utils::check_multi_thresholds(\%metrics, $warn_ref, $crit_ref, '%');


=cut

sub check_multi_thresholds {

    my $metrics = shift || die "Missing hash ref of metrics to check!";

    my $warning = shift || die "Missing array ref of warning thresholds!";

    my $critical   = shift || die "Missing array ref of critical thresholds!";
    my $type_label = shift || '';

    my $results = {
        'critical' => [],
        'warning'  => [],
        'ok'       => [],
        'perfdata' => ''
    };
    my $default_cthr;
    my $default_wthr;
    my @cthr_names;
    my @wthr_names;
    ( $default_cthr, $critical, @cthr_names ) = extract_thresholds($critical);
    ( $default_wthr, $warning,  @wthr_names ) = extract_thresholds($warning);
    $critical =
      rebuild_thresholds( $critical, $metrics, \@cthr_names, $default_cthr );
    $warning =
      rebuild_thresholds( $warning, $metrics, \@wthr_names, $default_wthr );
    my %checked;

    for my $c (@$critical) {

        my ( $metric, $op, $value ) = ( @{$c} );

        debug("check_multi_thresholds: check critical $metric $op $value");

        my $real   = $metrics->{$metric}->{'value'};
        my $result = eval_expr("$real $op $value");

        $checked{$metric}->{'critical'} = $value;

        if ( $result == 1 ) {

            if ( exists $metrics->{$metric}->{'label'} ) {

                my $metric_label = $metrics->{$metric}->{'label'};

                push(
                    @{ $results->{'critical'} },
                    "$metric ($real$metric_label $op $value$metric_label)"
                );

            }
            else {
                push(
                    @{ $results->{'critical'} },
                    "$metric ($real$type_label $op $value$type_label)"
                );
            }
            $checked{$metric}->{'caught'} = 1;
        }

    }

    for my $w (@$warning) {

        my ( $metric, $op, $value ) = (@$w);

        my $real = $metrics->{$metric}->{'value'};
        $checked{$metric}->{'warning'} = $value;

        debug("check_multi_thresholds: check warning $metric $op $value");

        next if exists $checked{$metric}->{'caught'};

        my $result = eval_expr("$real $op $value");

        if ( $result == 1 ) {
            if ( exists $metrics->{$metric}->{'label'} ) {

                my $metric_label = $metrics->{$metric}->{'label'};
                push(
                    @{ $results->{'warning'} },
                    "$metric ($real$metric_label $op $value$metric_label)"
                );

            }
            else {

                push(
                    @{ $results->{'warning'} },
                    "$metric ($real$type_label $op $value$type_label)"
                );
            }

            $checked{$metric}->{'caught'} = 1;
        }

    }

    my $perfdata;
    for my $metric ( sort keys %$metrics ) {

        my $w            = 0;
        my $metric_label = q{};

        ### If the metric has a defined {'label'} then use that otherwise
        ### used what was passed into the routine for $type_label
        if ( exists $metrics->{$metric}->{'label'} ) {
            $metric_label = $metrics->{$metric}->{'label'};
        }
        else {
            $metric_label = $type_label;
        }

        if ( exists $checked{$metric}->{'warning'} ) {
            $w = $checked{$metric}->{'warning'};
            $w = 0 unless $w =~ m/^[\d\.]+$/;
        }

        my $c = 0;

        if ( exists $checked{$metric}->{'critical'} ) {
            $c = $checked{$metric}->{'critical'};
            $c = 0 unless $c =~ m/^[\d\.]+$/;
        }

        $perfdata .=
          " '$metric'=$metrics->{$metric}->{'value'}" . "$metric_label;$w;$c";

        next if exists $checked{$metric}->{'caught'};

        my $value = $metrics->{$metric}->{'value'};
        push( @{ $results->{'ok'} }, "$metric $value$metric_label" );

    }

    $results->{'perfdata'} = $perfdata;

    return $results;

}

sub eval_expr {

    my $expr   = shift;
    my $result = 0;

    eval {
        $result = eval "($expr);";
        die $@ if $@;
    };

    $result = 0 if ( ( !defined $result ) or ( $result eq '' ) );

    debug("eval_expr: $expr returned $result");

    return $result;

}

sub rebuild_thresholds {

    my $thresholds        = shift;
    my $metrics           = shift;
    my $threshold_names   = shift;
    my $default_threshold = shift;

    if ( defined $default_threshold ) {
        for my $metric_index ( sort keys %$metrics ) {

            my $threshold_found = 0;

            $threshold_found =
              metric_has_threshold( $metric_index, $threshold_names );

            if ( $threshold_found == 0 ) {
                if ( defined $default_threshold ) {
                    $default_threshold->[0] = $metric_index;
                    my @copy_threshold = @$default_threshold;
                    push( @$thresholds, \@copy_threshold );
                }

            }

        }
    }
    return $thresholds;
}

sub extract_thresholds {
    my $thresholds = shift;
    my $default_threshold;

    my $size = scalar(@$thresholds);

    #If the threshold is named default, remove it from the thresholds and
    # return it separately as we use it for cases in which there is not
    # a specific threshold for the metric being checked

  CHECK_FOR_DEFAULT_THRESHOLD:
    for ( my $index = 0 ; $index < $size ; $index++ ) {
        if ( $thresholds->[$index]->[0] eq 'default' ) {
            $default_threshold = splice( @$thresholds, $index, 1 );
            last CHECK_FOR_DEFAULT_THRESHOLD;
        }
    }

    my @thresholds_names = ();
    foreach my $threshold (@$thresholds) {
        push( @thresholds_names, $threshold->[0] );

    }
    return ( $default_threshold, $thresholds, @thresholds_names );

}

sub metric_has_threshold {

    my $metric_name          = shift;
    my $threshold_names      = shift;
    my $threshold_found      = 0;
    my $threshold_names_size = scalar(@$threshold_names);
    if (   ( $threshold_names_size != 0 )
        && ( defined $threshold_names->[0] )
        && ( $threshold_names->[0] ne '' ) )
    {

      METRIC_FOUND_IN_THRESHOLDS:

        foreach my $threshold_name (@$threshold_names) {
            if ( $metric_name eq $threshold_name ) {
                $threshold_found = 1;
                last METRIC_FOUND_IN_THRESHOLDS;
            }
        }

    }
    return $threshold_found;

}

=pod

=head2 convert_to($type_symbol, $metrics_hash_ref)

    Convert all values in the 'value' keys of the hash passed in by reference
    to the type referenced by the $type_symbol passed in.  Valid values for
    $type_symbol are: 'n', '%', 'K', 'k', 'G', 'g', 'T', 't', 'M', or 'm'.  Large 
    K, G, M, T all will be computed using powers of 1024, lower case versions 
    will be multiplied by 1000 * N where K == 1, M == 2, G == 3, and T == 4.
    
    If percent is specified, the routine assumes that all passed in metrics 
    added together make up the total for the type of metric they represent.  
    Routine expects that 'raw' values will be in a key named 'raw' for 
    every metiric passed in, e.g.

    my $cpu_metrics = { 
        'nice' => { 'raw' = 2390239, 'value' => 0 
        'system' => { 'raw' = 23902390, 'value' => 0 
        'user' => { 'raw' = 949348984, 'value' => 0 
    };

        Nenm::Utils::convert_to('%', $cpu_metrics);

=cut

sub convert_to {

    my $convert_to  = shift;
    my $metrics_ref = shift;

    my $valid_types = '\%|B|M|K|G';

    die "Invalid metric type $convert_to passed in!"
      unless $convert_to =~ m/^${valid_types}$/i;

    if ( $convert_to eq '%' ) {

        my $total = 0;

        for my $metric ( keys %{$metrics_ref} ) {
            $total += $metrics_ref->{$metric}->{'raw'};
        }

        for my $m ( keys %{$metrics_ref} ) {

            if ( $total != 0 ) {
                $metrics_ref->{$m}->{'value'} = sprintf( "%.2f",
                    ( $metrics_ref->{$m}->{'raw'} / $total ) * 100 );
            }
            else {
                $metrics_ref->{$m}->{'value'} = 0;
            }
        }

    }
    else {

        my $base = 1024;
        $base = 1000 if ( $convert_to =~ /[a-z]/ );

        my $power = 0;

        $convert_to = lc($convert_to);

        if ( $convert_to eq 'b' ) {
            $power = 1;
        }
        elsif ( $convert_to eq 'm' ) {
            $power = 2;
        }
        elsif ( $convert_to eq 'g' ) {
            $power = 3;
        }
        elsif ( $convert_to eq 't' ) {
            $power = 4;
        }

        my $multiplier = $base**$power;

        for my $m ( keys %{$metrics_ref} ) {
            $metrics_ref->{$m}->{'value'} =
              $metrics_ref->{$m}->{'raw'} * $multiplier;
        }

    }

}

=pod

=head2 output_multi_results($label, $results_hash_ref);

Takes an Nagios plugin label along with the results as returned by
check_multi_thresholds and outputs results text, including perfdata.
For every result passed in, the most critical result wins; list of
all thresholds breached and all values that are ok is output in a
comma separated list, divided by label.  Example:


=cut

sub output_multi_results {

    my ( $label, $results, $opt_msg ) = (@_);
    $opt_msg = ( defined $opt_msg ) ? "- $opt_msg - " : '';

    my @critical = @{ $results->{'critical'} };
    my @warning  = @{ $results->{'warning'} };
    my @ok       = @{ $results->{'ok'} };

    my $level = OK;

    print "$label $opt_msg";

    if ( scalar(@critical) ) {
        print "CRITICAL - " . join( ', ', @critical ) . ' ';
        $level = CRITICAL;
    }

    if ( scalar(@warning) ) {
        print "WARNING - " . join( ', ', @warning ) . ' ';
        $level = WARNING unless $level == CRITICAL;
    }

    if ( scalar(@ok) ) {
        print "OK - " . join( ', ', @ok ) . ' ';
    }

    print " | $results->{'perfdata'}\n";

    return $level;

}

sub debug {

    return unless defined $Nenm::Utils::DEBUG;
    return unless $Nenm::Utils::DEBUG == 1;

    my $msg = shift;

    warn scalar( localtime() ) . ": $msg\n";
}

1;
