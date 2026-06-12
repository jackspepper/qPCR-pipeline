# -------------------------------------------------------------------------
# get_version.R - Standalone Script with Smart Path Integrity Verification
# -------------------------------------------------------------------------

verify_pipeline_integrity <- function(
    owner = "jackspepper", 
    repo = "qPCR-pipeline", 
    branch = "main", 
    changelog_path = "CHANGELOG.md",
    core_files = c("qpcr_cleaning_pipeline.R", "qpcr_consolidation.R")
) {
  
  # --- INTERNAL HELPER: Local Fallback (Offline Mode) ---
  get_local_fallback <- function(msg) {
    # Try to find CHANGELOG in root or R/
    actual_changelog <- if (file.exists(changelog_path)) changelog_path else if (file.exists(file.path("R", changelog_path))) file.path("R", changelog_path) else NULL
    
    warning(paste(msg, "Falling back to local metadata. Integrity verification skipped."))
    if (!is.null(actual_changelog)) {
      lines <- readLines(actual_changelog, warn = FALSE)
      target_line <- grep("## .* - current", lines, value = TRUE)
      if (length(target_line) > 0) {
        print(target_line)
        version_num <- sub("^##\\s*([0-9a-zA-Z.-]+)\\s*-.*$", "\\1", target_line[1])
        return(list(version = paste0("v", version_num, " (local-fallback)"), integrity_passed = "Fallback"))
      }
    }
    return(list(version = "v-unknown (offline)", integrity_passed = "Offline"))
  }
  
  # 1. Ensure 'gh' and 'digest' packages are available
  missing_pkgs <- c("gh", "digest")[!c(requireNamespace("gh", quietly = TRUE), requireNamespace("digest", quietly = TRUE))]
  if (length(missing_pkgs) > 0) {
    message("Initializing verification dependencies (one-time setup)...")
    tryCatch({
      install.packages(missing_pkgs, repos = "https://cloud.r-project.org", quiet = TRUE)
    }, error = function(e) {
      return(get_local_fallback("Failed to install required verification packages."))
    })
  }
  
  # 2. Query GitHub API for version and repository tree structure
  tryCatch({
    # Get latest commit for versioning
    commit_endpoint <- sprintf("GET /repos/%s/%s/commits/%s", owner, repo, branch)
    #latest_commit   <- gh::gh(commit_endpoint)
    sha_short       <- substring(latest_commit$sha, 1, 7)
    commit_date     <- as.Date(latest_commit$commit$committer$date)
    version_string  <- paste0("v-", branch, ".", sha_short, " (", commit_date, ")")
    
    # Get recursive repository tree structure so it finds files inside subfolders (like R/)
    tree_endpoint <- sprintf("GET /repos/%s/%s/git/trees/%s?recursive=1", owner, repo, branch)
    repo_tree     <- gh::gh(tree_endpoint)
    
    # 3. Check integrity of core scripts
    integrity_passed <- TRUE
    mismatched_files <- c()
    
    for (file in core_files) {
      # Smart Path Detection: Check root, then check R/ subfolder
      actual_path <- NULL
      if (file.exists(file)) {
        actual_path <- file
      } else if (file.exists(file.path("R", file))) {
        actual_path <- file.path("R", file)
      }
      
      if (!is.null(actual_path)) {
        # Find the matching file blob in GitHub's tree structure (matching just the filename)
        gh_file_info <- Filter(function(x) basename(x$path) == file && x$type == "blob", repo_tree$tree)
        
        if (length(gh_file_info) > 0) {
          expected_sha <- gh_file_info[[1]]$sha
          
          # Git calculates SHA-1 as: sha1("blob " + filesize + "\0" + file_contents)
          file_size  <- file.info(actual_path)$size
          file_con   <- file(actual_path, "rb")
          file_bytes <- readBin(file_con, "raw", n = file_size)
          close(file_con)
          
          git_blob_header <- c(charToRaw(paste0("blob ", file_size)), as.raw(0))
          local_sha <- digest::digest(c(git_blob_header, file_bytes), algo = "sha1", serialize = FALSE)
          
          if (local_sha != expected_sha) {
            integrity_passed <- FALSE
            mismatched_files <- c(mismatched_files, file)
          }
        }
      } else {
        warning(paste("Core pipeline file missing locally:", file))
        integrity_passed <- FALSE
      }
    }
    
    if (!integrity_passed && length(mismatched_files) > 0) {
      message(paste(
        "\nINTEGRITY WARNING: The following core scripts have been modified or corrupted:\n",
        paste("- ", mismatched_files, collapse = "\n"),
        "\nTo prevent unexpected data cleaning errors, avoid editing core scripts directly. Use the Runner script instead."
      ))
    }
    
    return(list(version = version_string, integrity_passed = integrity_passed))
    
  }, error = function(e) {
    return(get_local_fallback("Could not connect to GitHub API."))
  })
}

verify_pipeline_integrity()