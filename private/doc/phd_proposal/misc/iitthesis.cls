%%
%% iitthesis
%%
%% A class for authoring graduate degree theses
%% according to the specifications of the Technion -
%% Israeli Institute of Technology
%%
%%
%% Contributors:
%%
%% Dan Kenigsberg       <danken@cs.technion.ac.il>
%%       Author of the thesis on which the original class was based
%% Ido Tal              <idotal@cs.technion.ac.il>
%%                      <ital@mail.ucsd.edu>
%%       Original class author
%% Eyal Rozenberg       <eyalroz@technion.ac.il>
%%                      <eyalroz@riseup.net>
%%       Contributor
%%
%% Notes:
%% - This class has a hard-coded default of using A4 paper,11pt-size font
%%   for the body text, and 'one-and-a-half' line spacing.
%% - This class is still kind of half-baked. Please help improve it.
%%
%% To Do - Check recent changes:
%% - Make sure the openright option for the report class doesn't cause weird
%%   behavior with the Hebrew part
%% - Figure out why we can't set the had-figures label using AtEndDocument
%%   (we currently do it when setting the backmatter, as a workaround)
%%
%% To Do - Conformance with Technion regulations:
%% - Technion regulations require some text regarding the publications in
%%   which results from the thesis have been published. The regulations are
%%   extremely vague regarding the required layout of this information on
%%   the page, and somewhat vague and confusing regarding the content as well
%%   currently, there is a very rudimentary mechanism in place as part of this
%%   class to include such information. The iitthesis-extra style file
%%   includes an \butcheredbibliography commands which produces a
%%   mini-bibliography of just those cite keys passed to it - those keys
%%   relevant to meeting the requirement. See its use in the example thesis.
%% - The grad school and the Technion printing unit approve of the margins
%%   set here, but no official opinion/regulation on margin sizes has been
%%   issued. You should try and get them to issue it.
%% - Consider adding a Technion crest graphic somewhere (and possibly even
%%   faculty-specific crests). This is not a requirement, but I wonder
%%   whether the Technion doesn't want this somehow.
%%
%% To Do - LaTeX quality improvements:
%% - Make the class work with xelatex and polyglossia, not just
%%   pdf(e)latex and babel
%% - Consider switching from report class to something else (KOMAscript's
%%   scrreprt or perhaps memoir)
%% - There's code duplication for the front matter vs the front cover; we
%%   should have a single macro with an option for hiding what doesn't appear
%%   on the front cover
%% - Adding a Table of Contents makes the (first?) page on which it begins
%%   be numbered. We need to avoid that; for now we use an ugly workaround
%%   which should be replaced by an actual solution
%%
%% To Do - Robustness:
%% - Ensure the auto-list-of-figures works with the page reversal feature
%% - Use today's Hebrew and Gregorian month and year if the user does not
%%   specify the submission months/dates
%%
%% To Do - User experience & features:
%% - Allow use of the draft package
%% - Have user specify just the submission date, and calculate the Hebrew
%%   Gregorian months
%%   date is specified
%% - Make it so that when the class user does not define a notations &
%%   abbreviations ``chapter'', the class does not create it in the output -
%%   without requiring a noabbrevs option
%% - Give an informative error message when no degree is specified
%% - Allow for the optional inclusion of a Hebrew Table-of-Contents and
%%   List-of-Figures, even though these are no longer required by Technion
%%   regulations.
%% - Possible parts of a thesis not currently supported: Glossary; Index;
%%   Dedication (separate from acknowledgements); List of Tables
%% - Need to give more attentinon to the inclusion of figures
%% - Need to give more attentinon to the abbreviations page, maybe add an
%%   abbreviation macro and automagically generate the abbrevs page if
%%   necessary
%%
%% To Do - Miscellany:
%% - Perhaps move the code regarding publication info from
%%   iitthesis-extra.sty into here, and make the publication info listing
%%   default behavior of the class?
%% - Give more consideration to line and paragraph spacing (perhaps something
%%   more refined than setting \onehalfspacing ) ?
%% - When makefrontmatter'ing, and if we're creating a PDF, use \pdfinfo
%%   to set the PDF file's meta-data
%% - Unify the indentation in this file
%%

\def\filename{iitthesis}
\def\fileversion{v0.6}
\def\filedate{2012/03/10}
\NeedsTeXFormat{LaTeX2e}
\edef\iitthesis@TechnionIITThesis{%
\filedate\space\fileversion\space - Technion IIT Thesis}
\ProvidesClass{\filename}[\iitthesis@TechnionIITThesis]

%--------------------------------

% Packages and package option setting - before option processing

\RequirePackage{etoolbox}

%--------------------------------

% The different degrees the Technion awards for which you might
% be writing a thesis. At the moment, the author(s) are not aware of
% any difference in the style requirements for theses for
% different degrees

% A Ph.D. thesis is the default...
\newcommand{\iitthesis@degreeEnglish}{Doctor of Philosophy}
\newcommand{\iitthesis@degreeHebrew}{דוקטור לפילוסופיה}
\DeclareOption{PhD}{%
}

\DeclareOption{MSc}{%
  \renewcommand{\iitthesis@degreeEnglish}{Master of Science in \iitthesis@disciplineEnglish}
  \renewcommand{\iitthesis@degreeHebrew}{מגיסטר למדעים ב\relax\iitthesis@disciplineHebrew}
}

\DeclareOption{MArch}{%
  \renewcommand{\iitthesis@degreeEnglish}{Master of Architecture}
  \renewcommand{\iitthesis@degreeHebrew}{מגיסטר לארכיטקטורה}
}

\DeclareOption{MEcon}{%
  \renewcommand{\iitthesis@degreeEnglish}{Master of Economics}
  \renewcommand{\iitthesis@degreeHebrew}{מגיסטר לכלכלה}
}

\DeclareOption{ME}{%
  \renewcommand{\iitthesis@degreeEnglish}{Master of Engineering}
  \renewcommand{\iitthesis@degreeHebrew}{מגיסטר להנדסה}
}

\newbool{iitthesis-abbrevs}
\booltrue{iitthesis-abbrevs}
\DeclareOption{noabbrevs}{
  % No abbreviations & notiation section
  \boolfalse{iitthesis-abbrevs}
}

\newbool{iitthesis-includeack}
\booltrue{iitthesis-includeack}
\DeclareOption{beforeExam}{
  % No personal Acknowledgement are printed before the exam is taken and passed
  \boolfalse{iitthesis-includeack}
}

\newcommand{\iitthesis@geometry}{%
  \geometry{a4paper,nohead,twoside,top=2.5cm,bindingoffset=1.5cm,hmargin=2.5cm}}
\DeclareOption{fullpageDraft}{
 \newcommand{\iitthesis@fullpageDraft}{}
 \renewcommand{\iitthesis@geometry}{%
   \geometry{a4paper,nohead,twoside,bindingoffset=0cm,vmargin=2.5cm,hmargin=2.5cm}}
}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}

\ProcessOptions\relax

\LoadClass[a4paper,11pt,twoside,openright]{report}

%--------------------------------

% Packages and package option setting - after option processing

%\RequirePackage[hebrew,english]{babel}
\RequirePackage{setspace}
\RequirePackage{geometry}
\RequirePackage{atbegshi}

\iitthesis@geometry

%--------------------------------

%\doublespacing
\onehalfspacing

%--------------------------------


% A mechanism for determining whether the document contained
% any figures. Using this, we can decide whether or not we need
% a list of figures
% see: http://tex.stackexchange.com/q/33193/5640
%

\AtEndEnvironment{figure}{\gdef\iitthesis@havefigures{}}
% The following fails for some reason... moving it to the
% makebackmatter command
%\AtEndDocument{
% \ifdefined\iitthesis@havefigures
%  \label{iitthesis:had-figures}
% \fi
%}
\newcommand{\conditionallistoffigures}{
 \@ifundefined{r@iitthesis:had-figures}
 {}
 {%
  \listoffigures%
  \addcontentsline{cot}{chapter}{רשימת איורים}%
  \addcontentsline{toc}{chapter}{List of Figures}%
  }
}%

%--------------------------------

% Commands regarding the typesetting of the Hebrew material
% at the end of the thesis

% This commands flips the inner and outer margins; the motivation is
% that for Hebrew content, the binding is on the right side of the first
% page on the sheet, and on the left side of the second page on the sheet
% (not sure if I can call them recto and verso in this situation)
% see: http://tex.stackexchange.com/q/33193/5640
%
% The link also has code for when you have 'headings' page styles, which in
% general Technion theses should not

\newcommand*{\flipmargins}{%
  \clearpage
  \setlength{\@tempdima}{\oddsidemargin}%
  \setlength{\oddsidemargin}{\evensidemargin}%
  \setlength{\evensidemargin}{\@tempdima}%
  \if@reversemargin
    \normalmarginpar
  \else
    \reversemarginpar
  \fi
}

% see http://tex.stackexchange.com/q/21703/5640

\newif\ifRP%
\newbox\RPbox%
\setbox\RPbox\vbox{\vskip1pt}%
\AtBeginShipout{%
  \ifRP
    \AtBeginShipoutDiscard%
    \global\setbox\RPbox\vbox{\unvbox\RPbox
      \box\AtBeginShipoutBox\kern\c@page sp}%
  \fi
}%
\renewcommand{\RPtrue}{%
  \clearpage
  \ifRP\RPfalse\fi
  \global\let\ifRP\iftrue
}%
\let\reversepageorder=\RPtrue%
\renewcommand{\RPfalse}{%
  \clearpage
  \global\let\ifRP\iffalse
  \setbox\RPbox\vbox{\unvbox\RPbox
    \def\protect{\noexpand\protect\noexpand}%
    \@whilesw\ifdim0pt=\lastskip\fi
      {\c@page\lastkern\unkern\shipout\lastbox}%
  }%
}%
\let\resumenormalpageorder=\RPfalse%

%--------------------------------

% All the various data fields which the user must/should set, with
% generic/placeholder defaults
%
% for every thesis field, say titleEnglish, the class user (that means you
% who are writing your thesis) must invoke
% \titleEnglish{Non-linear Hummus Optimization}
% to replace the default;  every field has its own command, e.g. for the  name
% of your discipline in Hebrew you would use
% \disciplineHebrew{Bio-technical Engineering}
%
% Note that _all_ fields defined below are necessary, at the moment,
% to meet the Technion's thesis submission guidelines
%

%
\newcommand{\iitthesis@thesisdatafield}[2]{%
  % this defines the internal command which stores the field value
  \@namedef{iitthesis@#1}{#2}
  % this defines the command with which the class user will set the value
  \long\@namedef{#1}##1{\@namedef{iitthesis@#1}{##1}}
}

\iitthesis@thesisdatafield{authorEnglish}{Name of Author}
\iitthesis@thesisdatafield{authorHebrew}{שם המחבר}
\iitthesis@thesisdatafield{titleEnglish}{Thesis Title}
\iitthesis@thesisdatafield{titleHebrew}{כותרת החיבור}
\iitthesis@thesisdatafield{disciplineEnglish}{Discipline Name}
\iitthesis@thesisdatafield{disciplineHebrew}{שם התחום המדעי}
\iitthesis@thesisdatafield{supervisionEnglish}{Information regarding the supervision and supervisors}
\iitthesis@thesisdatafield{supervisionHebrew}{מידע על המנחים וההנחיה}
\iitthesis@thesisdatafield{publicationInfoHebrew}{ציון אם העבודה פורסמה בכתבי עת או  הוצגה בכנסים. הרשימה תכתב בהתאם לכללי הציטוט (כולל הכותר ושמות השותפים). במקרה של שיתוף פעולה, פירוט תרומת המגיש.}
\iitthesis@thesisdatafield{publicationInfoEnglish}{Info about whether the contents of the thesis was published, about collaborators and the thesis author's contribution.}
\iitthesis@thesisdatafield{GregorianDateEnglish}{Gregorian Month and Year}
\iitthesis@thesisdatafield{GregorianDateHebrew}{חודש ושנה גרגוריאניים}
\iitthesis@thesisdatafield{JewishDateEnglish}{Hebrew Month and Year}
\iitthesis@thesisdatafield{JewishDateHebrew}{חודש ושנה עבריים}
\iitthesis@thesisdatafield{personalAcknowledgementEnglish}{The author's personal acknowledgements.}
\iitthesis@thesisdatafield{personalAcknowledgementHebrew}{שלמי-התודה של המחבר.}
\iitthesis@thesisdatafield{financialAcknowledgementEnglish}{The generous financial help of the Technion is gratefully acknowledged.}
\iitthesis@thesisdatafield{financialAcknowledgementHebrew}{אני מודה לטכניון על התמיכה הכספית הנדיבה בהשתלמותי.}
% Regulations require the English abstract contain 200-500 words.
\iitthesis@thesisdatafield{abstractEnglish}{Abstract of the thesis.}
% Regulations require the Hebrew abstract contain 1,000-2,000 words.
\iitthesis@thesisdatafield{abstractHebrew}{תקציר החיבור.}
\iitthesis@thesisdatafield{abbreviationsAndNotation}{Abbreviations and
notations listing here.} \iitthesis@thesisdatafield{thesisbibstyle}{alpha}
% the following should be a comma-separate list of relative filenames,
% e.g. {backmatter/compsci.bib,backmatter/math.bib,specific.bib}
\iitthesis@thesisdatafield{thesisbibfiles}{}

%--------------------------------

% These next two commands are for the class user to invoke:


\newcommand{\makefrontmatter}{%

 \pagestyle{empty}

 \makecoverEnglish
 \cleardoublepage
 \maketitleEnglish
 \cleardoublepage
% \makeacknowledgementsEnglish
% \cleardoublepage

 % Workaround: The first page of the table of contents is getting numbered,
 % although it's not supposed to be (pagestyle empty); so let's at least
 % change the numbering style so that we don't actually get the number...
 \emptypagenumbering
 \tableofcontents
 \cleardoublepage
 \conditionallistoffigures
 \cleardoublepage

 % Regulations require that numbering starts on the first page
 % of the abstract, being page 1
 \pagestyle{plain}
 \pagenumbering{arabic}
 \setcounter{page}{1}
 \makeabstractEnglish
% \cleardoublepage

 \ifbool{iitthesis-abbrevs}{
  \makeabbreviationsAndNotation
  \cleardoublepage
 }{ }
}

\newcommand{\makebibliography}{%
  \ifx\iitthesis@thesisbibfiles\@empty
    \PackageError{iitthesis}{%
      No bibliography files specified; a thesis must contain a bibliography.}
  \else
    \ifdefined\pdfbookmark
      % If hyperref is loaded, this will help us to more
      % easily navigate to the bibliography
      \pdfbookmark[0]{Bibliography}{bib}%
    \fi
    \bibliographystyle{\iitthesis@thesisbibstyle}%
    \bibliography{\iitthesis@thesisbibfiles}%
  \fi%
}

\newcommand{\makebackmatter}{%

 % This _should_ be done AtEndDocument, but that fails for some reason
 \ifdefined\iitthesis@havefigures
  \label{iitthesis:had-figures}
 \fi

 \clearpage
 \makebibliography

 % Note that the order of pages here will in fact be
 % the order of pages when opening the bound thesis from
 % the ``Hebrew side'', i.e. from the back cover of the English side
 % which is the front cover for the Hebrew side; thus
 % after printing, you have to take all the pages starting
 % from the Hebrew cover page, as a packet, and reverse their direction

 % We do not make the bibliography here - for now...

 \clearpage
 \pagestyle{empty}

 % We are now on the next page after the end of the main matter. We wish
 % to begin the Hebrew content - but we don't want to do this on the same
 % sheet, with English on the recto side and Hebrew on the verso; the Hebrew
 % content pages are added to the PDF in reverse direction and we wish to
 % begin 'cleanly' with the English sheets complete and left alone, so we
 % just clear the sheet
 \cleardoublepage

 \reversepageorder
   % reversing the output page order from this point on

 % The first Hebrew page must appear with the binding to its right; but the
 % way things are so far in the document is that odd pages (recto) have
 % margins to the left. So... maybe we should \cleartoleftpage (i.e. clear
 % to the next verso)? No, this will not to. Why? For two reasons.
 %
 % First, remember the page order is reversed. The last page we get on the
 % output, being the first page we set here, must be the Hebrew cover page,
 % not a blank page.
 %
 % Second, if we \cleartoleftpage, this will mess up the  page ordering.
 % If the Hebrew pages are:
 %
 % Title verso, Empty recto, IntroPage1 verso, IntroPage2 recto
 %
 % then a \cleartoleftpage would give us the following sheets:
 %
 % Empty recto + Title verso , Empty recto + IntroPage1 verso,
 % IntroPage2 recto + nothing/empty verso
 %
 % which is not what we wanted: We get a gratuitious empty page between
 % pages 1 and 2 of the intro... so we actually have no choice but to flip
 % the margins

 \flipmargins

 \makecoverHebrew
 % We're not clearing to a verso page, since we've flipped the margins;
 % this should generate 1 empty page between the cover page and the title page
 \cleardoublepage
 \maketitleHebrew
 \cleardoublepage
 \makeacknowledgementsHebrew
 \cleardoublepage

 \pagestyle{plain}
 % We need to reset the displayed page number, but we don't want to
 % change the "real" page number, specifically for verso/recto margin
 % setting reasons; see:
 % http://tex.stackexchange.com/q/36011/5640
 %
 \displayedpagenumbering{roman}{1}

 \makeabstractHebrew
 % For some reason, if you \clearpage after the Hebrew abstract, the
 % \AtEndDocument command which is part of the auto-list-of-figures
 % generation is not run; so commenting this out
 %\clearpage

 % Consider using the solution here:
 % http://tex.stackexchange.com/q/21703/5640
 %\usepackage{atbegshi}
 % and reverse the ordering of the Hebrew sheets

 \resumenormalpageorder
    % now outputting the reversed pages and resuming normal order
    % (although we have no pages left...)

}

%--------------------------------

% Commands which the class user should not directly invoke; they
% are used in \makefrontmatter and \makebackmatter
%

\newcommand{\makecoverEnglish}{%
% Front cover / cover page
 \begin{center}
  \pagestyle{empty}

  \vspace*{\stretch{1}}
  \begin{spacing}{1}
    \Huge \bf
%    \scalebox{0.95}{\iitthesis@titleEnglish}
    \iitthesis@titleEnglish
  \end{spacing}
  \Large

  \vspace*{\stretch{1}}

  Ph.D. Research Proposal

  \vspace*{\stretch{0.5}}

  \vphantom{\parbox{4in}{
    \normalsize
    \begin{center}
%      Submitted in partial fulfillment of the requirements \\
%      for the degree of \iitthesis@degreeEnglish
    \end{center}
  }}


  \vspace*{\stretch{2}}

  {\LARGE \bf \iitthesis@authorEnglish}

  \vspace*{\stretch{1}}

  {\LARGE Prof. Yoav Etsion}

  \vspace*{\stretch{3}}

  \parbox{3.5in}{
    \normalsize
    \begin{center}
%      Submitted to the Senate \\
%      of the Technion --- Israel Institute of Technology
%      \iitthesis@JewishDateEnglish \hspace{1cm} Haifa \hspace{1cm} 
    \iitthesis@GregorianDateEnglish
    \end{center}
  }
 \end{center}
}

\newcommand{\maketitleEnglish}{%
 \begin{center}

  \vspace*{\stretch{1}}
  \begin{spacing}{1}
    \Huge \bf \iitthesis@titleEnglish
  \end{spacing}
  \Large

  \vspace*{\stretch{1}}

  Ph.D. Research Proposal

  \vspace*{\stretch{0.5}}

  \parbox{4in}{
    \normalsize
    \begin{center}
%      Submitted in partial fulfillment of the requirements \\
%      for the degree of \iitthesis@degreeEnglish
    \end{center}
  }

  \vspace*{\stretch{2}}

  {\LARGE \bf \iitthesis@authorEnglish}
  
  \vspace*{\stretch{1}}

  {\LARGE Prof. Yoav Etsion}

  \vspace*{\stretch{3}}

  \parbox{3.5in}{
    \normalsize
    \begin{center}
%      Submitted to the Senate \\
%      of the Technion --- Israel Institute of Technology
%      \iitthesis@JewishDateEnglish \hspace{1cm} Haifa \hspace{1cm} 
\iitthesis@GregorianDateEnglish
    \end{center}
  }
 \end{center}
}

\newcommand{\makeacknowledgementsEnglish}{%

  \noindent \iitthesis@supervisionEnglish

  \ifx\iitthesis@publicationInfoEnglish\@empty
  \else
    \medskip
    \noindent \iitthesis@publicationInfoEnglish

  \fi
  \ifbool{iitthesis-includeack}{
    %\vspace*{\stretch{1}}
    \medskip

    \begin{center}
    \textbf{\Large \textsc{Acknowledgements}}
    \end{center}
    \noindent
    \iitthesis@personalAcknowledgementEnglish

    \smallskip
    \vspace*{\stretch{2}}
  }{
    \vspace*{\stretch{1}}
  }
  \noindent \iitthesis@financialAcknowledgementEnglish
}

\newcommand{\makeabstractEnglish}{%
 \chapter*{Abstract}
 \addcontentsline{toc}{chapter}{Abstract}
 \addcontentsline{cot}{chapter}{תקציר}

 \iitthesis@abstractEnglish
}

\newcommand{\makeabbreviationsAndNotation}{%
 \chapter*{Abbreviations and Notations}
 \addcontentsline{toc}{chapter}{Abbreviations and Notations}
 \addcontentsline{cot}{chapter}{רשימת קיצורים וסמלים}
 \iitthesis@abbreviationsAndNotation
}

% Like \cleardoublepage, but instead of getting you to the next recto
% page, it gets you to the next verso page; see:
% http://tex.stackexchange.com/q/11707/5640
\newcommand*{\cleartoleftpage}{%
  \clearpage
    \if@twoside
    \ifodd\c@page
      \hbox{}\newpage
      \if@twocolumn
        \hbox{}\newpage
      \fi
    \fi
  \fi
}

% Workaround for the verso -> switch triggered by our restarting of numbering;
% see:
% http://tex.stackexchange.com/q/36011/5640
%
% example:
% \displayedpagenumbering}{roman}{9}
% will make the page number printed at the bottom of the page be IX in Roman
% numerals (with X on the next page, then XI etc.) - but will not change the
% value of the 'page' counter.
%
\newcommand{\displayedpagenumbering}[2]{%
  \edef\@tempcntval{\@arabic{\value{page}}}
  \long\@namedef{@#1withoffset}##1{\csname @#1\endcsname{\numexpr ##1 - \@tempcntval + #2\relax}}%
  \pagenumbering{#1withoffset}%
  \setcounter{page}{\@tempcntval}%
}

% This is used in the workaround for the first ToC page being numbered
% without our requesting it to be
\newcommand{\emptypagenumbering}{%
  \@namedef{@emptynumbering}##1{\relax}%
  \pagenumbering{emptynumbering}%
}


\newcommand{\makecoverHebrew}{

 \begin{otherlanguage}{hebrew}
 \begin{center}
   \vspace*{\stretch{1}}
   \begin{spacing}{1}
     \Huge \bf \iitthesis@titleHebrew
   \end{spacing}
   \Large

   \vspace*{\stretch{1}}

   \vphantom{חיבור על מחקר}

   \vspace*{\stretch{0.5}}

   \vphantom{\parbox{3.5in}{
     \normalsize
     \begin{center}
       לשם מילוי חלקי של הדרישות לקבלת התואר \\
       \iitthesis@degreeHebrew
     \end{center}
  }}

  \vspace*{\stretch{2}}

  {\LARGE \bf \iitthesis@authorHebrew}

  \vspace*{\stretch{3}}

  \vphantom{\parbox{3in}{
     \normalsize
     \begin{center}
       מוגש לסנט הטכניון --- מכון טכנולוגי לישראל
       \iitthesis@JewishDateHebrew
       \hspace{0.75cm} חיפה \hspace{0.75cm}
       \iitthesis@GregorianDateHebrew
     \end{center}
  }}
 \end{center}
 \end{otherlanguage}
}

\newcommand{\maketitleHebrew}{
 \begin{otherlanguage}{hebrew}
 \begin{center}

  \vspace*{\stretch{1}}
  \begin{spacing}{1}
    \Huge \bf \iitthesis@titleHebrew
  \end{spacing}
  \Large

  \vspace*{\stretch{1}}

  חיבור על מחקר

  \vspace*{\stretch{0.5}}

  \parbox{3.5in}{
    \normalsize
    \begin{center}
      לשם מילוי חלקי של הדרישות לקבלת התואר \\
      \iitthesis@degreeHebrew
    \end{center}
  }

  \vspace*{\stretch{2}}

  {\LARGE \bf \iitthesis@authorHebrew}

  \vspace*{\stretch{3}}

  \parbox{4in}{
     \normalsize
     \begin{center}
       הוגש לסנט הטכניון --- מכון טכנולוגי לישראל \\
       \iitthesis@JewishDateHebrew
       \hspace{0.75cm} חיפה \hspace{0.75cm}
       \iitthesis@GregorianDateHebrew
     \end{center}
  }
 \end{center}
 \end{otherlanguage}
}

\newcommand{\makeacknowledgementsHebrew}{
  \begin{otherlanguage}{hebrew}
  \noindent \iitthesis@supervisionHebrew

  \ifx\iitthesis@publicationInfoHebrew\@empty
  \else
    \medskip
    \noindent \iitthesis@publicationInfoHebrew

  \fi

  \ifbool{iitthesis-includeack}{
    %\vspace*{\stretch{1}}
    \medskip
    \begin{center}
    \textbf{\Large \textsc{תודות}}
    \end{center}
    \noindent
    \iitthesis@personalAcknowledgementHebrew

    \vspace*{\stretch{2}}
  }{
    \vspace*{\stretch{1}}
  }
  \noindent \iitthesis@financialAcknowledgementHebrew
  \end{otherlanguage}
}

\newcommand{\makeabstractHebrew}{
  \begin{otherlanguage}{hebrew}
  \chapter*{תקציר}
  \addcontentsline{cot}{chapter}{תקציר עברי}
  \addcontentsline{toc}{chapter}{Hebrew Abstract}
  \setlength{\parindent}{0pt}
  \setlength{\parskip}{10pt}

  \iitthesis@abstractHebrew

  \end{otherlanguage}
}

% ----------------------------------------------------------
%
% Commands which may be of some use in the main matter (and are
% not used in the front or back matter)
%

% The following commented-out commands have been suggested by Ido Tal,
% to overcome problems with the two-language sectioning commands; the
% pre-existing commands (which should be available - at least with LaTeX2e
% and babel, perhaps requiring some package inclusion) are:
%  \bpart{hebrew}{english}
%  \bchapter{hebrew}{english}
%  \bsection{hebrew}{english}
%  \bsubsection{hebrew}{english}
%  \bsubsubsection{hebrew}{english}
%  \bcaption{hebrew}{english}
%  \bchapternn{hebrew}{english} - replaces \chapter*
%  \bsectionnn{hebrew}{english} - replaces \section*


% Ido Tal says:
% This is a hack. The proper thing to do would be to get \section* to work.
% For some reason, it doesn't, and I don't know how to fix this.
% Eyal asks:
% Doesn't bsectionnn work?
\newcommand{\bsectionstar}[2]{
 \section*{#2}
 \addcontentsline{toc}{section}{#2}
 \addcontentsline{cot}{section}{#1}
}

% Ido Tal suggests the following commands to fix problems with captions


\endinput
