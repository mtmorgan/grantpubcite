test_that("versions are 'known'", {
    ## NIH RePORTER has versions in the last page of the PDF at
    ## <https://api.reporter.nih.gov/documents/Data%20Elements%20for%20RePORTER%20Project%20API_V2.pdf>,
    ## but I'm not sure how to access this...

    ## iCite does not have obvious version numbers. It would still be
    ## useful to capture the data release, but that is also not
    ## available from iCite per se.
})

test_that("RePORTER PROJECTS endpoint exists", {
    skip_if_offline()

    ## minimal query, a single record
    url <- REPORTER_PROJECTS
    full_query <- '{"criteria":{"foa":["RFA-CA-19-039"]},"offset":0,"limit":1}'
    expect_silent(response <- post(url, full_query, httr::content_type_json()))

    expect_identical(
        jmespath(response, "keys(@)", as = "R"),
        c("meta", "results")
    )

    meta_keys <- c(
        "search_id", "total", "offset", "limit", "sort_field",
        "sort_order", "sorted_by_relevance", "properties"
    )
    expect_true(setequal(
        jmespath(response, "meta.keys(@)", as = "R"),
        meta_keys
    ))

    ## fields returned 11 December, 2023
    projects_keys <- c(
        "appl_id", "subproject_id", "fiscal_year", "project_num",
        "project_serial_num", "organization", "award_type",
        "activity_code", "award_amount", "is_active",
        "project_num_split", "principal_investigators",
        "contact_pi_name", "program_officers", "agency_ic_admin",
        "agency_ic_fundings", "cong_dist", "spending_categories",
        "project_start_date", "project_end_date", "organization_type",
        "opportunity_number", "full_study_section",
        "award_notice_date", "is_new", "mechanism_code_dc",
        "core_project_num", "terms", "pref_terms", "abstract_text",
        "project_title", "phr_text", "spending_categories_desc",
        "agency_code", "covid_response", "arra_funded",
        "budget_start", "budget_end", "cfda_code",
        "funding_mechanism", "direct_cost_amt", "indirect_cost_amt",
        "project_detail_url", "date_added"
    )

    expect_true(setequal(
        jmespath(response, "results[0].keys(@)", as = "R"),
        projects_keys
    ))
})

test_that("NIH RePORTER PUBLICATIONS endpoint exists", {
    skip_if_offline()

    url <- REPORTER_PUBLICATIONS
    full_query <- '{"criteria":{"foa":["RFA-CA-19-039"]},"offset":0,"limit":1}'
    expect_silent(response <- post(url, full_query, httr::content_type_json()))

    expect_identical(
        jmespath(response, "keys(@)", as = "R"),
        c("meta", "results", "facet_results")
    )

    meta_keys <- c(
        "search_id", "total", "offset", "limit", "sort_field",
        "sort_order", "sorted_by_relevance", "properties"
    )
    expect_true(setequal(
        jmespath(response, "meta.keys(@)", as = "R"),
        meta_keys
    ))

    results_keys <- c("coreproject", "pmid", "applid")
    expect_true(setequal(
        jmespath(response, "results[0].keys(@)", as = "R"),
        results_keys
    ))
})

test_that("iCITE PUBS endpoint exists", {
    skip_if_offline()

    ## example is from top of https://icite.od.nih.gov/api
    url <- sprintf("%s?%s", ICITE_PUBS, "pmids=28968381")
    expect_silent(response <- get(url, httr::content_type_json()))

    expect_true(setequal(
        jmespath(response, "keys(@)", as = "R"),
        c("meta", "links", "data")
    ))

    expect_identical(
        jmespath(response, "meta", as = "R"),
        list(pmids = "28968381")
    )

    expect_identical(
        jmespath(response, "links", as = "R"),
        list(self = "https://icite.od.nih.gov/api/pubs?pmids=28968381")
    )

    ## fields returned 11 December, 2023
    data_keys <- c(
        "pmid", "year", "title", "authors", "journal",
        "is_research_article", "relative_citation_ratio",
        "nih_percentile", "human", "animal", "molecular_cellular",
        "apt", "is_clinical", "citation_count", "citations_per_year",
        "expected_citations_per_year", "field_citation_rate",
        "provisional", "x_coord", "y_coord", "cited_by_clin",
        "cited_by", "references", "doi", "last_modified"
    )
    expect_true(setequal(
        jmespath(response, "data[0].keys(@)", as = "R"),
        data_keys
    ))
})
