# ------------------------------------------------------------------
# Git & GitHub Academy - a Shiny app that teaches git commands and
# GitHub workflows.
#
# Install once:  install.packages(c("shiny", "bslib", "DT"))
# Run:           shiny::runApp("app.R")   (or click "Run App" in RStudio)
# ------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)

# ---------------------------- helpers ------------------------------

code_block <- function(...) {
  tags$pre(class = "bg-dark text-light p-3 rounded",
           tags$code(paste(c(...), collapse = "\n")))
}

tip <- function(...) {
  div(class = "alert alert-info py-2", icon("lightbulb"), " ", ...)
}

warn <- function(...) {
  div(class = "alert alert-warning py-2", icon("triangle-exclamation"), " ", ...)
}

# ------------------------- cheat-sheet data ------------------------

cheats <- data.frame(
  Category = c(
    rep("Setup", 4), rep("Snapshot", 7), rep("Branching", 6),
    rep("Remote", 6), rep("Inspect", 4), rep("Undo", 6), rep("Stash/Tags", 4)
  ),
  Command = c(
    "git config --global user.name \"Your Name\"",
    "git config --global user.email \"you@example.com\"",
    "git init",
    "git clone <url>",
    
    "git status", "git add <file>", "git add .", "git commit -m \"msg\"",
    "git commit --amend", "git rm <file>", "git mv <old> <new>",
    
    "git branch", "git branch <name>", "git switch <name>",
    "git switch -c <name>", "git merge <name>", "git branch -d <name>",
    
    "git remote -v", "git remote add origin <url>", "git push -u origin <branch>",
    "git fetch", "git pull", "git push",
    
    "git log --oneline --graph --all", "git diff", "git diff --staged",
    "git show <commit>",
    
    "git restore <file>", "git restore --staged <file>",
    "git reset --soft HEAD~1", "git reset --hard <commit>",
    "git revert <commit>", "git reflog",
    
    "git stash", "git stash pop", "git tag v1.0.0", "git rebase main"
  ),
  Description = c(
    "Set the name attached to your commits.",
    "Set the email attached to your commits.",
    "Turn the current folder into a git repository.",
    "Copy a remote repository (with history) to your machine.",
    
    "Show modified, staged and untracked files.",
    "Stage a specific file for the next commit.",
    "Stage every change in the current folder.",
    "Record the staged snapshot with a message.",
    "Rewrite the last commit (message or content). Avoid after pushing.",
    "Delete a file and stage the deletion.",
    "Rename/move a file and stage it.",
    
    "List local branches (* marks the current one).",
    "Create a new branch at the current commit.",
    "Move to another branch.",
    "Create a branch and move to it in one step.",
    "Merge the named branch into the current branch.",
    "Delete a branch that is already merged.",
    
    "List the remotes and their URLs.",
    "Link your local repo to a GitHub repo called 'origin'.",
    "Upload a branch and remember it for future pushes.",
    "Download remote changes without merging them.",
    "Fetch and merge remote changes into your branch.",
    "Upload your local commits to the remote.",
    
    "Compact, graphical history of every branch.",
    "Show unstaged changes.",
    "Show staged changes (what the next commit will contain).",
    "Show the details of one commit.",
    
    "Discard unstaged changes in a file (cannot be undone).",
    "Unstage a file but keep your edits.",
    "Undo the last commit, keep changes staged.",
    "Move the branch to <commit> and DISCARD all later work.",
    "Create a new commit that undoes <commit> (safe for shared history).",
    "Log of where HEAD has been - your safety net.",
    
    "Shelve uncommitted work temporarily.",
    "Re-apply the most recently stashed work.",
    "Label the current commit (e.g. a release).",
    "Replay your branch on top of main for a linear history."
  ),
  stringsAsFactors = FALSE
)

# ---------------------------- quiz data ----------------------------

quiz <- list(
  list(q = "Which command records your staged changes as a permanent snapshot?",
       choices = c("git push", "git commit", "git add", "git fetch"),
       answer = "git commit",
       why = "git add stages, git commit records the snapshot locally, git push uploads it."),
  list(q = "You want to create AND switch to a new branch called 'feature-x'. Which command?",
       choices = c("git branch feature-x", "git switch feature-x",
                   "git switch -c feature-x", "git merge feature-x"),
       answer = "git switch -c feature-x",
       why = "-c means 'create'. git branch alone creates but does not switch."),
  list(q = "What is the difference between git fetch and git pull?",
       choices = c("They are identical",
                   "fetch downloads changes; pull downloads AND merges them",
                   "pull downloads; fetch merges",
                   "fetch uploads; pull downloads"),
       answer = "fetch downloads changes; pull downloads AND merges them",
       why = "git pull = git fetch + git merge (or rebase)."),
  list(q = "In the GitHub Flow, how do changes get reviewed before reaching main?",
       choices = c("Push straight to main", "Open a Pull Request from a branch",
                   "Email a patch", "Use git reset --hard"),
       answer = "Open a Pull Request from a branch",
       why = "Branch -> commits -> Pull Request -> review -> merge -> delete branch."),
  list(q = "You already pushed a bad commit to a shared branch. What is the SAFEST fix?",
       choices = c("git reset --hard then force push", "git revert <commit>",
                   "Delete the repo", "git commit --amend and force push"),
       answer = "git revert <commit>",
       why = "revert adds a new commit that undoes the bad one without rewriting shared history."),
  list(q = "In the fork-and-pull-request workflow, what is 'upstream'?",
       choices = c("Your personal fork", "The original repository you forked from",
                   "Your local folder", "A branch called upstream"),
       answer = "The original repository you forked from",
       why = "'origin' is usually your fork; 'upstream' is the original project.")
)

# ------------------------------ UI ---------------------------------

ui <- page_navbar(
  title = span(icon("code-branch"), " Git & GitHub Academy"),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,
  header = tags$head(tags$style(HTML("
    pre { font-size: 0.9rem; }
    .lesson { max-width: 960px; margin: auto; padding-top: 1rem; }
    #sim_console { background:#1e1e1e; color:#d4d4d4; font-family:monospace;
                   min-height:260px; max-height:360px; overflow-y:auto; }
  "))),
  
  # ------------------------------ Home -------------------------------
  nav_panel("Start here", icon = icon("house"),
            div(class = "lesson",
                h2("Learn Git & GitHub by doing"),
                p("Git is a version-control system that tracks every change to your files.",
                  "GitHub is a website that hosts git repositories so you can back up your work,",
                  "collaborate, and review code. This app walks you from your first commit to",
                  "team workflows."),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  card(card_header("1. Learn"), "Work through the lesson tabs in order."),
                  card(card_header("2. Practice"), "Use the Command Builder and the Git Sandbox."),
                  card(card_header("3. Test"), "Take the quiz and keep the cheat sheet handy.")
                ),
                h4("Mental model"),
                layout_columns(
                  col_widths = c(3, 3, 3, 3),
                  card(card_header("Working directory"), "Files you edit."),
                  card(card_header("Staging area"), "Changes you chose for the next commit (git add)."),
                  card(card_header("Local repository"), "Saved snapshots (git commit)."),
                  card(card_header("Remote (GitHub)"), "Shared copy (git push / pull).")
                ),
                code_block("edit files -> git add -> git commit -> git push",
                           "  (working)    (staging)   (local repo)   (GitHub)"),
                tip("Commits are snapshots, not differences. Each commit points to its parent,",
                    "forming a chain; branches are just movable labels on that chain.")
            )
  ),
  
  # ------------------------------ Setup ------------------------------
  nav_panel("1. Setup", icon = icon("gear"),
            div(class = "lesson",
                h3("Install and configure git"),
                p("Download git from git-scm.com, then tell git who you are (one time per computer):"),
                code_block('git --version',
                           'git config --global user.name "Your Name"',
                           'git config --global user.email "you@example.com"',
                           'git config --global init.defaultBranch main',
                           'git config --list'),
                h4("Connect to GitHub"),
                tags$ol(
                  tags$li("Create a free account at github.com."),
                  tags$li("Authenticate. Easiest options: the GitHub CLI (", tags$code("gh auth login"),
                          ") or a Personal Access Token stored by a credential manager."),
                  tags$li("Or use SSH keys: ", tags$code('ssh-keygen -t ed25519 -C "you@example.com"'),
                          " then add the public key in GitHub > Settings > SSH keys.")
                ),
                warn("GitHub no longer accepts your account password for git over HTTPS. Use a token, SSH or the GitHub CLI."),
                h4("Two ways to start a repository"),
                code_block("# A) brand new project",
                           "mkdir my-project && cd my-project",
                           "git init",
                           "",
                           "# B) copy an existing project from GitHub",
                           "git clone https://github.com/<user>/<repo>.git")
            )
  ),
  
  # --------------------------- Core workflow -------------------------
  nav_panel("2. Core workflow", icon = icon("floppy-disk"),
            div(class = "lesson",
                h3("The everyday loop"),
                code_block("git status                  # what changed?",
                           "git add analysis.R          # stage one file",
                           "git add .                   # ...or stage everything",
                           'git commit -m "Add data cleaning step"',
                           "git log --oneline           # review history"),
                accordion(
                  open = "Writing good commit messages",
                  accordion_panel("Writing good commit messages",
                                  tags$ul(
                                    tags$li("Use the imperative mood: 'Fix bug', not 'Fixed bug'."),
                                    tags$li("Keep the first line under about 50 characters."),
                                    tags$li("Make small, focused commits - one logical change each."),
                                    tags$li("Explain WHY in the body if it is not obvious.")
                                  ),
                                  code_block('git commit -m "Fix NA handling in summary table" -m "Rows with missing age were silently dropped."')
                  ),
                  accordion_panel("Seeing what changed",
                                  code_block("git diff                # unstaged changes",
                                             "git diff --staged       # staged changes",
                                             "git show HEAD           # the last commit",
                                             "git log --oneline --graph --all")
                  ),
                  accordion_panel("Ignoring files with .gitignore",
                                  p("List files git should never track (secrets, large data, build output)."),
                                  code_block(".Rproj.user/", ".Rhistory", ".RData", ".Renviron", "*.csv", "data/raw/")
                  )
                )
            )
  ),
  
  # ----------------------------- Branching ---------------------------
  nav_panel("3. Branching", icon = icon("code-branch"),
            div(class = "lesson",
                h3("Branches let you experiment safely"),
                p("A branch is an independent line of development. 'main' holds stable work;",
                  "you do new work on separate branches and merge when ready."),
                code_block("git branch                    # list branches",
                           "git switch -c feature/plots   # create + switch",
                           "# ...edit, add, commit...",
                           "git switch main",
                           "git merge feature/plots       # bring the work in",
                           "git branch -d feature/plots   # clean up"),
                h4("Merge conflicts"),
                p("A conflict happens when two branches change the same lines. Git pauses and marks the file:"),
                code_block("<<<<<<< HEAD",
                           "title <- 'Version on main'",
                           "=======",
                           "title <- 'Version on feature branch'",
                           ">>>>>>> feature/plots"),
                tags$ol(
                  tags$li("Open the file and choose / combine the correct lines; delete the markers."),
                  tags$li(tags$code("git add <file>"), " to mark it resolved."),
                  tags$li(tags$code("git commit"), " to finish the merge.")
                ),
                tip("Escape hatch: ", tags$code("git merge --abort"), " returns you to before the merge."),
                h4("Merge vs rebase"),
                layout_columns(
                  col_widths = c(6, 6),
                  card(card_header("merge"), "Preserves history exactly, adds a merge commit. Safe default."),
                  card(card_header("rebase"), "Replays your commits on top of another branch for a linear history. Never rebase commits others already pulled.")
                )
            )
  ),
  
  # --------------------------- GitHub basics -------------------------
  nav_panel("4. Remotes & GitHub", icon = icon("github"),
            div(class = "lesson",
                h3("Working with a remote"),
                p("A remote is a copy of the repository hosted elsewhere. By convention the main one is called ", tags$code("origin"), "."),
                h4("Publish a local project to GitHub"),
                code_block("# 1. create an EMPTY repo on github.com (no README)",
                           "git remote add origin https://github.com/<user>/<repo>.git",
                           "git branch -M main",
                           "git push -u origin main     # -u remembers the link"),
                h4("Stay in sync"),
                code_block("git fetch            # download, don't change my files",
                           "git pull             # fetch + merge",
                           "git push             # upload my commits"),
                warn("If push is rejected ('non-fast-forward'), someone pushed first. Run ", tags$code("git pull"), ", resolve any conflicts, then push again."),
                h4("GitHub features worth knowing"),
                tags$ul(
                  tags$li(tags$b("Issues"), " - track bugs and tasks."),
                  tags$li(tags$b("Pull Requests"), " - propose, discuss and review changes before merging."),
                  tags$li(tags$b("Actions"), " - automate tests and deployments."),
                  tags$li(tags$b("Branch protection"), " - require reviews/checks before merging to main."),
                  tags$li(tags$b("Releases & Tags"), " - package stable versions.")
                )
            )
  ),
  
  # ----------------------------- Workflows ---------------------------
  nav_panel("5. GitHub workflows", icon = icon("diagram-project"),
            div(class = "lesson",
                h3("Collaboration workflows"),
                p("Pick the workflow that fits your team size and release style."),
                accordion(
                  open = "GitHub Flow (recommended starting point)",
                  accordion_panel("GitHub Flow (recommended starting point)",
                                  p("Simple, continuous delivery. 'main' is always deployable."),
                                  code_block("git switch main && git pull",
                                             "git switch -c fix/typo-in-readme",
                                             "# edit + commit (small, focused)",
                                             "git push -u origin fix/typo-in-readme",
                                             "# open a Pull Request on GitHub -> review -> merge",
                                             "git switch main && git pull",
                                             "git branch -d fix/typo-in-readme"),
                                  tags$ol(tags$li("Branch from main"), tags$li("Commit"), tags$li("Open Pull Request"),
                                          tags$li("Review & discuss"), tags$li("Merge"), tags$li("Delete branch"))
                  ),
                  accordion_panel("Fork & Pull Request (open-source contributions)",
                                  p("You don't have write access to the project, so you work on your own copy (a fork)."),
                                  code_block("# 1. Click 'Fork' on GitHub",
                                             "git clone https://github.com/<you>/<project>.git",
                                             "cd <project>",
                                             "git remote add upstream https://github.com/<owner>/<project>.git",
                                             "",
                                             "# keep your fork current",
                                             "git fetch upstream",
                                             "git switch main",
                                             "git merge upstream/main",
                                             "git push origin main",
                                             "",
                                             "# contribute",
                                             "git switch -c my-improvement",
                                             "# edit + commit",
                                             "git push -u origin my-improvement",
                                             "# open Pull Request: your fork -> upstream"),
                                  tip("origin = your fork, upstream = the original project.")
                  ),
                  accordion_panel("Gitflow (scheduled releases)",
                                  p("Uses long-lived branches: ", tags$code("main"), " (released), ",
                                    tags$code("develop"), " (integration), plus ", tags$code("feature/*"),
                                    ", ", tags$code("release/*"), " and ", tags$code("hotfix/*"), "."),
                                  code_block("git switch develop",
                                             "git switch -c feature/new-model",
                                             "# ...work...  merge back into develop via PR",
                                             "git switch -c release/1.2.0 develop",
                                             "# final fixes, then merge into main and tag",
                                             "git tag -a v1.2.0 -m 'Release 1.2.0'"),
                                  p("Heavier process; best for versioned software with scheduled releases.")
                  ),
                  accordion_panel("Pull Request etiquette & code review",
                                  tags$ul(
                                    tags$li("Keep PRs small and focused; describe what and why."),
                                    tags$li("Link issues: write 'Closes #12' in the description."),
                                    tags$li("Respond to review comments by pushing more commits to the same branch."),
                                    tags$li("Merge options: Merge commit, Squash (one tidy commit) or Rebase."),
                                    tags$li("Never commit passwords, tokens or sensitive data.")
                                  )
                  )
                )
            )
  ),
  
  # ------------------------------- Undo ------------------------------
  nav_panel("6. Undo mistakes", icon = icon("rotate-left"),
            div(class = "lesson",
                h3("How to fix things"),
                p("Choose by asking: has it been committed? has it been pushed?"),
                datatable(
                  data.frame(
                    Situation = c("Edited a file, want the old version back",
                                  "Staged the wrong file",
                                  "Typo in last commit message (not pushed)",
                                  "Last commit was premature (not pushed)",
                                  "Bad commit already pushed / shared",
                                  "Need to throw away ALL local work",
                                  "Lost a commit after a reset"),
                    Fix = c("git restore <file>",
                            "git restore --staged <file>",
                            "git commit --amend -m \"new message\"",
                            "git reset --soft HEAD~1",
                            "git revert <commit>",
                            "git reset --hard HEAD",
                            "git reflog, then git switch -c rescue <hash>"),
                    Safety = c("Destructive", "Safe", "Safe if unpushed", "Safe if unpushed",
                               "Safe for shared history", "Destructive", "Safe")
                  ),
                  rownames = FALSE, options = list(dom = "t", pageLength = 10)
                ),
                br(),
                warn(tags$code("git reset --hard"), " and ", tags$code("git push --force"),
                     " can permanently destroy work. Prefer ", tags$code("git revert"),
                     " on shared branches.")
            )
  ),
  
  # ------------------------------ From R -----------------------------
  nav_panel("7. Git from R", icon = icon("r-project"),
            div(class = "lesson",
                h3("Using git and GitHub from R / RStudio"),
                p("The usethis and gert packages let you do the whole workflow from the R console."),
                code_block('install.packages(c("usethis", "gert", "gh"))',
                           '',
                           '# one-time setup',
                           'usethis::use_git_config(user.name = "Your Name", user.email = "you@example.com")',
                           'usethis::create_github_token()   # opens GitHub to make a token',
                           'gitcreds::gitcreds_set()         # paste the token',
                           'usethis::git_sitrep()            # diagnose your setup',
                           '',
                           '# new project -> GitHub',
                           'usethis::create_project("~/my-analysis")',
                           'usethis::use_git()',
                           'usethis::use_github()',
                           '',
                           '# everyday git with gert',
                           'gert::git_status()',
                           'gert::git_add(".")',
                           'gert::git_commit("Add cleaning script")',
                           'gert::git_push()',
                           'gert::git_log(max = 5)',
                           '',
                           '# pull request workflow',
                           'usethis::pr_init("fix-typo")     # create branch',
                           'usethis::pr_push()               # push + open PR',
                           'usethis::pr_finish()             # merge cleanup'),
                tip("In RStudio, the Git pane (top-right) offers Stage, Commit, Pull and Push buttons.",
                    "Use renv alongside git to version your package environment too.")
            )
  ),
  
  # ----------------------------- Cheat sheet -------------------------
  nav_panel("Cheat sheet", icon = icon("table-list"),
            div(class = "lesson",
                h3("Searchable command reference"),
                selectInput("cheat_cat", "Filter by category",
                            choices = c("All", unique(cheats$Category))),
                DTOutput("cheat_table")
            )
  ),
  
  # --------------------------- Command builder -----------------------
  nav_panel("Command builder", icon = icon("screwdriver-wrench"),
            div(class = "lesson",
                h3("Build a command"),
                layout_columns(
                  col_widths = c(5, 7),
                  card(
                    selectInput("b_cmd", "What do you want to do?",
                                choices = c("Commit changes" = "commit",
                                            "Create a branch" = "branch",
                                            "Switch branch" = "switch",
                                            "Push to GitHub" = "push",
                                            "View history" = "log",
                                            "Undo something" = "undo")),
                    uiOutput("b_inputs")
                  ),
                  card(
                    card_header("Your command"),
                    verbatimTextOutput("b_out"),
                    uiOutput("b_explain")
                  )
                )
            )
  ),
  
  # ------------------------------ Sandbox ----------------------------
  nav_panel("Git sandbox", icon = icon("terminal"),
            div(class = "lesson",
                h3("Practice git safely"),
                p("This is a simulated git repository - nothing touches your computer.",
                  "Try: ", tags$code("git init"), ", click 'Edit a file', ",
                  tags$code("git add ."), ", ", tags$code('git commit -m "first"'), ", ",
                  tags$code("git switch -c feature"), ", more commits, ",
                  tags$code("git switch main"), ", ", tags$code("git merge feature"), "."),
                layout_columns(
                  col_widths = c(6, 6),
                  card(
                    card_header("Terminal"),
                    div(id = "sim_console", class = "p-2 rounded", uiOutput("sim_log")),
                    layout_columns(
                      col_widths = c(9, 3),
                      textInput("sim_cmd", NULL, placeholder = "type a git command and press Enter",
                                width = "100%"),
                      actionButton("sim_run", "Run", class = "btn-primary w-100")
                    ),
                    div(
                      actionButton("sim_edit", "Edit a file", icon = icon("pen"),
                                   class = "btn-outline-secondary btn-sm"),
                      actionButton("sim_reset", "Reset sandbox", icon = icon("trash"),
                                   class = "btn-outline-danger btn-sm")
                    ),
                    tags$small(class = "text-muted",
                               "Supported: init, status, add, commit -m, branch, switch [-c], checkout [-b], merge, log --oneline, help, clear")
                  ),
                  card(
                    card_header("Commit graph"),
                    plotOutput("sim_plot", height = "340px"),
                    verbatimTextOutput("sim_state")
                  )
                ),
                tags$script(HTML("
        $(document).on('keypress', '#sim_cmd', function(e) {
          if (e.which == 13) { $('#sim_run').click(); }
        });
        Shiny.addCustomMessageHandler('scrollConsole', function(x) {
          var el = document.getElementById('sim_console');
          if (el) el.scrollTop = el.scrollHeight;
        });
      "))
            )
  ),
  
  # ------------------------------- Quiz ------------------------------
  nav_panel("Quiz", icon = icon("circle-question"),
            div(class = "lesson",
                h3("Check your understanding"),
                lapply(seq_along(quiz), function(i) {
                  radioButtons(paste0("quiz_", i), paste0(i, ". ", quiz[[i]]$q),
                               choices = quiz[[i]]$choices, selected = character(0),
                               width = "100%")
                }),
                actionButton("quiz_submit", "Check answers", class = "btn-success"),
                br(), br(),
                uiOutput("quiz_result")
            )
  ),
  
  nav_spacer(),
  nav_item(tags$a(href = "https://git-scm.com/doc", target = "_blank", "Official docs"))
)

# ---------------------------- server -------------------------------

new_state <- function() {
  list(init = FALSE, commits = list(), branches = list(), head = NA_character_,
       modified = FALSE, staged = FALSE, counter = 0L)
}

ancestors_of <- function(commits, id) {
  seen <- character()
  stack <- id
  while (length(stack) > 0) {
    cur <- stack[1]
    stack <- stack[-1]
    if (is.na(cur) || cur %in% seen) next
    seen <- c(seen, cur)
    stack <- c(stack, commits[[cur]]$parents)
  }
  seen
}

server <- function(input, output, session) {
  
  # ---------------------------- cheat sheet --------------------------
  output$cheat_table <- renderDT({
    d <- cheats
    if (input$cheat_cat != "All") d <- d[d$Category == input$cheat_cat, ]
    datatable(d, rownames = FALSE, filter = "none",
              options = list(pageLength = 15, dom = "ftip"))
  })
  
  # --------------------------- command builder -----------------------
  output$b_inputs <- renderUI({
    switch(input$b_cmd,
           commit = tagList(
             textInput("b_msg", "Commit message", "Add data cleaning script"),
             checkboxInput("b_all", "Stage all tracked changes first (-a)", FALSE)),
           branch = tagList(
             textInput("b_name", "New branch name", "feature/new-plot"),
             checkboxInput("b_switch", "Switch to it immediately", TRUE)),
           switch = textInput("b_name", "Branch to switch to", "main"),
           push = tagList(
             textInput("b_branch", "Branch", "main"),
             checkboxInput("b_u", "Set upstream (-u), first push of this branch", TRUE)),
           log = radioButtons("b_log", "Style",
                              c("One line per commit" = "oneline",
                                "Graph of all branches" = "graph",
                                "With changed files" = "stat",
                                "Last 5 commits" = "five")),
           undo = radioButtons("b_undo", "I want to...",
                               c("Discard edits to a file" = "restore",
                                 "Unstage a file" = "unstage",
                                 "Undo last commit, keep my work" = "soft",
                                 "Safely undo a pushed commit" = "revert"))
    )
  })
  
  builder <- reactive({
    switch(input$b_cmd,
           commit = {
             req(input$b_msg)
             list(cmd = sprintf('git commit %s-m "%s"', if (isTRUE(input$b_all)) "-a " else "",
                                input$b_msg),
                  why = "Records a snapshot of the staged changes. With -a, tracked files are staged automatically (new files still need git add).")
           },
           branch = {
             req(input$b_name)
             if (isTRUE(input$b_switch))
               list(cmd = paste("git switch -c", input$b_name),
                    why = "Creates the branch at your current commit and moves you onto it.")
             else
               list(cmd = paste("git branch", input$b_name),
                    why = "Creates the branch but leaves you where you are.")
           },
           switch = {
             req(input$b_name)
             list(cmd = paste("git switch", input$b_name),
                  why = "Changes your working files to match that branch. Commit or stash uncommitted work first.")
           },
           push = {
             req(input$b_branch)
             list(cmd = paste("git push", if (isTRUE(input$b_u)) "-u" else "", "origin", input$b_branch) |>
                    gsub(pattern = "  +", replacement = " "),
                  why = "Uploads your commits to GitHub. -u links the local and remote branch so later you can just type git push.")
           },
           log = {
             req(input$b_log)
             switch(input$b_log,
                    oneline = list(cmd = "git log --oneline", why = "Compact history: short hash + message."),
                    graph = list(cmd = "git log --oneline --graph --all", why = "Draws branches and merges in the terminal."),
                    stat = list(cmd = "git log --stat", why = "Shows which files each commit touched."),
                    five = list(cmd = "git log -5 --oneline", why = "Only the latest five commits."))
           },
           undo = {
             req(input$b_undo)
             switch(input$b_undo,
                    restore = list(cmd = "git restore <file>", why = "Throws away unstaged edits. Cannot be undone."),
                    unstage = list(cmd = "git restore --staged <file>", why = "Removes the file from the staging area; edits stay."),
                    soft = list(cmd = "git reset --soft HEAD~1", why = "Moves the branch back one commit; changes stay staged. Only for unpushed commits."),
                    revert = list(cmd = "git revert <commit-hash>", why = "Adds a new commit that reverses the old one - safe for shared branches."))
           })
  })
  
  output$b_out <- renderText(builder()$cmd)
  output$b_explain <- renderUI(div(class = "alert alert-secondary", builder()$why))
  
  # ------------------------------ sandbox ----------------------------
  st <- reactiveVal(new_state())
  term <- reactiveVal(c("Welcome! Type 'help' to see supported commands."))
  
  say <- function(...) term(c(term(), ...))
  
  observeEvent(input$sim_reset, {
    st(new_state())
    term("Sandbox reset. Type 'git init' to begin.")
  })
  
  observeEvent(input$sim_edit, {
    s <- st()
    if (!s$init) {
      say("(no repository yet - run 'git init' first)")
    } else {
      s$modified <- TRUE
      st(s)
      say("# you edited analysis.R")
    }
  })
  
  observeEvent(input$sim_run, {
    cmd <- trimws(input$sim_cmd)
    updateTextInput(session, "sim_cmd", value = "")
    if (cmd == "") return()
    say(paste0("$ ", cmd))
    s <- st()
    out <- character()
    
    if (cmd == "clear") {
      term(character())
      return()
    }
    if (cmd == "help") {
      out <- c("git init | git status | git add . | git commit -m \"msg\"",
               "git branch [name] | git switch [-c] name | git checkout [-b] name",
               "git merge name | git log --oneline | clear")
    } else if (!grepl("^git( |$)", cmd)) {
      out <- "Commands must start with 'git' (try 'help')."
    } else if (cmd == "git init") {
      if (s$init) {
        out <- "Reinitialized existing Git repository."
      } else {
        s$init <- TRUE; s$head <- "main"; s$branches <- list(main = NA_character_)
        out <- "Initialized empty Git repository (default branch: main)."
      }
    } else if (!s$init) {
      out <- "fatal: not a git repository. Run 'git init' first."
    } else if (cmd == "git status") {
      out <- paste("On branch", s$head)
      if (s$staged) out <- c(out, "Changes to be committed:", "\tmodified: analysis.R")
      else if (s$modified) out <- c(out, "Changes not staged for commit:", "\tmodified: analysis.R")
      else out <- c(out, "nothing to commit, working tree clean")
    } else if (grepl("^git add( |$)", cmd)) {
      if (s$modified) { s$staged <- TRUE; s$modified <- FALSE }
      else out <- "nothing to add (click 'Edit a file' first)."
    } else if (grepl("^git commit", cmd)) {
      m <- regmatches(cmd, regexec("^git commit\\s+-m\\s+[\"']?(.*?)[\"']?\\s*$", cmd))[[1]]
      if (length(m) < 2 || m[2] == "") {
        out <- "Use: git commit -m \"your message\""
      } else if (!s$staged) {
        out <- "nothing added to commit (edit a file, then 'git add .')."
      } else {
        s$counter <- s$counter + 1L
        id <- sprintf("%07x", sample.int(2^27, 1))
        parent <- s$branches[[s$head]]
        s$commits[[id]] <- list(id = id, msg = m[2], branch = s$head, idx = s$counter,
                                parents = if (is.na(parent)) character() else parent)
        s$branches[[s$head]] <- id
        s$staged <- FALSE
        out <- sprintf("[%s %s] %s", s$head, id, m[2])
      }
    } else if (grepl("^git branch", cmd)) {
      nm <- trimws(sub("^git branch", "", cmd))
      if (nm == "") {
        out <- paste0(ifelse(names(s$branches) == s$head, "* ", "  "), names(s$branches))
      } else if (nm %in% names(s$branches)) {
        out <- paste0("fatal: a branch named '", nm, "' already exists.")
      } else if (is.na(s$branches[[s$head]])) {
        out <- "fatal: no commits yet - make a commit first."
      } else {
        s$branches[[nm]] <- s$branches[[s$head]]
        out <- paste0("Created branch '", nm, "'.")
      }
    } else if (grepl("^git (switch|checkout)", cmd)) {
      create <- grepl("^git (switch -c|checkout -b) ", cmd)
      nm <- trimws(sub("^git (switch -c|checkout -b|switch|checkout)\\s*", "", cmd))
      if (nm == "") {
        out <- "Specify a branch name."
      } else if (create) {
        if (nm %in% names(s$branches)) out <- paste0("fatal: branch '", nm, "' already exists.")
        else if (is.na(s$branches[[s$head]])) out <- "fatal: make a commit before creating branches."
        else {
          s$branches[[nm]] <- s$branches[[s$head]]; s$head <- nm
          out <- paste0("Switched to a new branch '", nm, "'")
        }
      } else if (!nm %in% names(s$branches)) {
        out <- paste0("fatal: invalid reference: ", nm)
      } else if (s$modified || s$staged) {
        out <- "error: you have uncommitted changes. Commit them first (or stash)."
      } else {
        s$head <- nm
        out <- paste0("Switched to branch '", nm, "'")
      }
    } else if (grepl("^git merge ", cmd)) {
      nm <- trimws(sub("^git merge", "", cmd))
      cur <- s$branches[[s$head]]
      if (!nm %in% names(s$branches)) {
        out <- paste0("merge: ", nm, " - not something we can merge")
      } else if (nm == s$head || is.na(s$branches[[nm]])) {
        out <- "Already up to date."
      } else {
        tgt <- s$branches[[nm]]
        if (!is.na(cur) && tgt %in% ancestors_of(s$commits, cur)) {
          out <- "Already up to date."
        } else if (is.na(cur) || cur %in% ancestors_of(s$commits, tgt)) {
          s$branches[[s$head]] <- tgt
          out <- c("Updating...", "Fast-forward")
        } else {
          s$counter <- s$counter + 1L
          id <- sprintf("%07x", sample.int(2^27, 1))
          s$commits[[id]] <- list(id = id, msg = paste0("Merge branch '", nm, "'"),
                                  branch = s$head, idx = s$counter, parents = c(cur, tgt))
          s$branches[[s$head]] <- id
          out <- c("Merge made by the 'ort' strategy.", paste0("[", s$head, " ", id, "]"))
        }
      }
    } else if (grepl("^git log", cmd)) {
      tip_id <- s$branches[[s$head]]
      if (is.na(tip_id)) {
        out <- paste0("fatal: your current branch '", s$head, "' has no commits yet")
      } else {
        ids <- ancestors_of(s$commits, tip_id)
        ids <- ids[order(-vapply(ids, function(i) s$commits[[i]]$idx, numeric(1)))]
        out <- vapply(ids, function(i) {
          deco <- names(s$branches)[vapply(s$branches, function(b) identical(b, i), logical(1))]
          if (length(deco) && i == tip_id) deco <- sub(paste0("^", s$head, "$"), paste0("HEAD -> ", s$head), deco)
          paste0(i, if (length(deco)) paste0(" (", paste(deco, collapse = ", "), ")") else "",
                 " ", s$commits[[i]]$msg)
        }, character(1))
      }
    } else {
      out <- "That command isn't supported in this sandbox (type 'help')."
    }
    
    st(s)
    if (length(out)) say(out)
    session$sendCustomMessage("scrollConsole", TRUE)
  })
  
  output$sim_log <- renderUI({
    lines <- term()
    tags$div(lapply(lines, function(l) tags$div(HTML(gsub(" ", "&nbsp;", htmltools::htmlEscape(l))))))
  })
  
  output$sim_state <- renderText({
    s <- st()
    if (!s$init) return("No repository. Run: git init")
    br <- vapply(names(s$branches), function(b) {
      id <- s$branches[[b]]
      paste0(if (b == s$head) "* " else "  ", b, " -> ", if (is.na(id)) "(no commits)" else id)
    }, character(1))
    paste0("Working tree: ", if (s$staged) "staged changes" else if (s$modified) "modified" else "clean",
           "\n", paste(br, collapse = "\n"))
  })
  
  output$sim_plot <- renderPlot({
    s <- st()
    par(mar = c(1, 6, 1, 1))
    if (length(s$commits) == 0) {
      plot.new()
      text(0.5, 0.5, "Commit graph appears here\n(make your first commit)", col = "grey40", cex = 1.2)
      return()
    }
    cm <- s$commits
    df <- data.frame(id = names(cm),
                     idx = vapply(cm, function(x) x$idx, numeric(1)),
                     br = vapply(cm, function(x) x$branch, character(1)),
                     msg = vapply(cm, function(x) x$msg, character(1)),
                     stringsAsFactors = FALSE)
    lanes <- unique(df$br[order(df$idx)])
    all_lanes <- unique(c(lanes, names(s$branches)))
    df$y <- match(df$br, all_lanes)
    plot(NA, xlim = c(0.5, max(df$idx) + 1), ylim = c(0.4, length(all_lanes) + 0.9),
         axes = FALSE, xlab = "", ylab = "")
    axis(2, at = seq_along(all_lanes), labels = all_lanes, las = 1, tick = FALSE)
    for (i in seq_len(nrow(df))) {
      for (p in cm[[df$id[i]]]$parents) {
        pi <- df[df$id == p, ]
        segments(pi$idx, pi$y, df$idx[i], df$y[i], lwd = 2, col = "grey50")
      }
    }
    points(df$idx, df$y, pch = 21, bg = "#2c7be5", col = "white", cex = 3)
    text(df$idx, df$y - 0.3, df$msg, cex = 0.75)
    for (b in names(s$branches)) {
      id <- s$branches[[b]]
      if (is.na(id)) next
      r <- df[df$id == id, ]
      lab <- if (b == s$head) paste0("HEAD -> ", b) else b
      text(r$idx, r$y + 0.35, lab, cex = 0.8, font = 2,
           col = if (b == s$head) "#e63757" else "#00a86b")
    }
  })
  
  # -------------------------------- quiz -----------------------------
  observeEvent(input$quiz_submit, {
    res <- lapply(seq_along(quiz), function(i) {
      ans <- input[[paste0("quiz_", i)]]
      ok <- !is.null(ans) && identical(ans, quiz[[i]]$answer)
      div(class = paste("alert", if (ok) "alert-success" else "alert-danger", "py-2"),
          tags$b(paste0("Q", i, ": ", if (ok) "Correct. " else "Not quite. ")),
          quiz[[i]]$why,
          if (!ok) tagList(" Answer: ", tags$code(quiz[[i]]$answer)))
    })
    score <- sum(vapply(seq_along(quiz), function(i) {
      identical(input[[paste0("quiz_", i)]], quiz[[i]]$answer)
    }, logical(1)))
    output$quiz_result <- renderUI(tagList(
      h4(paste0("Score: ", score, " / ", length(quiz))), res))
  })
}

shinyApp(ui, server)