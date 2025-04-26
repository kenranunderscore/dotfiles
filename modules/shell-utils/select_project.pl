#!/usr/bin/env perl

use strict;
use warnings;

my @project_dirs = (
    "$ENV{HOME}/projects",
    "$ENV{HOME}/ag",
    "$ENV{HOME}/tmpdev",
);

my @projects;

sub is_git_repo {
    my ($dir) = @_;
    return -d "$dir/.git" || -f "$dir/.git";
}

sub rel_to_home {
    my ($abs) = @_;
    $abs =~ s{^$ENV{HOME}/?}{};
    return $abs;
}

sub with_prefix {
    my ($label, $text) = @_;
    my $prefix = "[$label]";
    my $indent = 6 - length($prefix);
    $indent = 0 if $indent < 0;
    return $prefix . ' ' x $indent . $text;
}

sub as_repo {
    my ($p) = @_;
    return with_prefix("R", $p);
}

sub as_worktree {
    my ($p) = @_;
    return with_prefix("T", $p);
}

push @projects, {
    label => as_repo("dotfiles"),
    path => "$ENV{HOME}/dotfiles",
};

for my $root (@project_dirs) {
    opendir(my $dh, $root) or next;
    while (my $entry = readdir($dh)) {
        next if $entry =~ /^\./;
        my $sub = "$root/$entry";
        next unless -d $sub;

        if (is_git_repo($sub)) {
            push @projects, {
                label => as_repo(rel_to_home($sub)),
                path => $sub,
            };
        } else {
            opendir(my $sdh, $sub) or next;
            while (my $wt = readdir($sdh)) {
                next if $wt =~ /^\./;
                my $p = "$sub/$wt";
                if (-d $p && is_git_repo($p)) {
                    push @projects, {
                        label => as_worktree(rel_to_home($p)),
                        path => $p,
                    };
                }
            }
            closedir($sdh)
        }
    }
    closedir($dh);
}

my $choices = join("\n", map { $_->{label} } @projects);
my $selection = `printf "$choices" | fzf --prompt="Navigate to project: "`;
chomp($selection);

if ($selection ne '') {
    for my $proj (@projects) {
        if ($proj->{label} eq $selection) {
            my $target = $proj->{path};
            print "$target\n";
            exit 0;
        }
    }
}

exit 1;
