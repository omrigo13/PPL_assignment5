:- module('ex5',
        [author/2,
         genre/2,
         book/4
        ]).

/*
 * **********************************************
 * Printing result depth
 *
 * You can enlarge it, if needed.
 * **********************************************
 */
maximum_printing_depth(100).
:- current_prolog_flag(toplevel_print_options, A),
   (select(max_depth(_), A, B), ! ; A = B),
   maximum_printing_depth(MPD),
   set_prolog_flag(toplevel_print_options, [max_depth(MPD)|B]).



author(1, "Isaac Asimov").
author(2, "Frank Herbert").
author(3, "William Morris").
author(4, "J.R.R Tolkein").


genre(1, "Science").
genre(2, "Literature").
genre(3, "Science Fiction").
genre(4, "Fantasy").

book("Inside The Atom", 1, 1, 500).
book("Asimov's Guide To Shakespeare", 1, 2, 400).
book("I, Robot", 1, 3, 450).
book("Dune", 2, 3, 550).
book("The Well at the World's End", 3, 4, 400).
book("The Hobbit", 4, 4, 250).
book("The Lord of the Rings", 4, 4, 1250).

% You can add more facts.
% Fill in the Purpose, Signature as requested in the instructions here

% Signature: authorOfGenre(GenreName, AuthorName)/2
% Purpose: true if an author by the name {AuthorName} has written a book
% belonging to the genre named {GenreName}
authorOfGenre(GenreName, AuthorName) :-
	genre(GenreId, GenreName),
	author(AuthorId, AuthorName),
	book(_A, AuthorId, GenreId, _B).

% Signature: longestBook(AuthorId, BookName)/2
% Purpose: true if the longest book that an author with the ID
% {AuthorId} has written in titled {BookName}
longestBook(AuthorId, BookName) :-
	book(BookName, AuthorId, _A, Length),
	not(longerBook(AuthorId, Length)).

% Signature: longerBook(AuthorId, Length)/2
% Purpose: true if there is a longer book by the author with
% the ID {AuthorId}
longerBook(AuthorId, Length) :-
    book(_B, AuthorId, _C, newLength),
    (newLength > Length).

% Signature: versatileAuthor(AuthorName)/1
% Purpose: true if an author by the name {AuthorName} has written books
% in at least three different genres
versatileAuthor(AuthorName) :-
    author(AuthorId, AuthorName),
    book(_A, AuthorId, FirstGenre, _B),
    book(_C, AuthorId, SecondGenre, _D),
    book(_E, AuthorId, ThirdGenre, _F),
    notEqualGenres(FirstGenre, SecondGenre, ThirdGenre).

% Signature: notEqualGenres(FirstGenre, SecondGenre, ThirdGenre)/3
% Purpose: true if the genres of 3 books is different
% equal
notEqualGenres(FirstGenreId, SecondGenreId, ThirdGenreId) :-
    (FirstGenreId =\= SecondGenreId),
    (SecondGenreId =\= ThirdGenreId),
    (ThirdGenreId =\= FirstGenreId).
