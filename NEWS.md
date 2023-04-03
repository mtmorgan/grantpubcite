# grantpubcite v0.0.2

- (v. 0.0.1.9001) Add `gpc_*()` helper functions to standardize
  the user API, processing of reporter return values, and shorten
  column names during display. 
  
- (v. 0.0.1.9001) Add `program_()` functions to provide a higher-level
  interface to collecting program-level information on projects and
  publications.
  
- (v. 0.0.1.9001) Add `copublication*()` and `cocitation*()` to
  summarize project-level collaboratoin at the publication and
  citation level.
  
- (v. 0.0.1.9001) improve datatable display with 'top' vertical
  alignment of cells, and truncating text to a maximum of 40
  characters.
  
- (v. 0.0.1.9001) Update IOTN and ITCR case studies to use new
  functionality.

- (v 0.0.1.9002) add `copublication_data()`

- (v 0.0.1.9002) change return value columns of `copublication()`
  - `collab`: number of collaborators, across all publications
  - `n_collab`: number of collaborative publications
  - `citn_collab`: total citation count of collaborartive publications
  
- (v 0.0.1.9003) add co-publication network to ITCR case study.

# grantpubcite v0.0.1

- (v. 0.0.1) Initial version. Don't show case study code by default,
  so the reports are more readble.

- (v. 0.0.0.9014) Add 'Implementation notes' to share enabling
  sources.

- (v. 0.0.0.9012) Add IOTN case study.

- (v. 0.0.0.9012) Add 'fnd' (funding) role to DESCRIPTION,
  acknowledging NIH NCI ITCR U24CA180996, and additional minor changes
  to articles.

- (v. 0.0.0.9011) Correct 'Innovative' / 'Early-stage' maturing
  order.

- (v. 0.0.0.9010) Update 'Getting started' link and number of projects
  mentioned in summary of ITCR case study. Thanks Juli Klemm.

- (v. 0.0.0.9009) Add missing FOAs (RFA-*) from 2019+; now 146 grants
   with more evidence of project maturation. Thanks to Satish
   Viswanath for pointing out the missing FOAs.

- (v. 0.0.0.9008) Add downloadable projects data. Thanks to Andrey
  Fedorov for comments leading to this and the preceeding change.

- (v. 0.0.0.9007) Add downloadable table of all ITCR publications /
  citations. Thanks Andrey Fedorov for the prompt.

- (v. 0.0.0.9006) Provide higher-level narrative summary to 'Case
  study: ITCR' article.

- (v. 0.0.0.9005) Limit number of results returned by `reporter_*()`
  to 10,000; larger result sets have in my experience been the result
  of mis-specification of query criteria.

- (v. 0.0.0.9005) Add a short article exploring specific project
  numbers.

- (v. 0.0.0.9005) Add a `NEWS.md` file to track changes to the
  package.
