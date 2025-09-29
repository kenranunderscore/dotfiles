local function as_projects(path)
  if vim.uv.fs_stat(path .. "/.git") then
    return { { path = path, type = "git" } }
  elseif vim.uv.fs_stat(path .. "/.pijul") then
    return { { path = path, type = "pijul" } }
  elseif vim.uv.fs_stat(path .. "/.svn") then
    return { { path = path, type = "subversion" } }
  elseif vim.uv.fs_stat(path .. "/flake.nix") then
    return { { path = path, type = "flake" } }
  else
    local worktrees = {}
    local handle = vim.uv.fs_scandir(path)
    if handle then
      while true do
        local name, type = vim.uv.fs_scandir_next(handle)
        if not name then
          break
        end

        if type == "directory" then
          local wt = path .. "/" .. name
          if vim.uv.fs_stat(wt .. "/.git") then
            table.insert(worktrees, { path = wt, type = "worktree" })
          end
        end
      end
    end
    return worktrees
  end
end

local function find_projects_in(base_dir)
  local projects = {}
  local expanded_dir = vim.fn.expand(base_dir)

  if not vim.uv.fs_stat(expanded_dir) then
    return projects
  end

  local handle = vim.uv.fs_scandir(expanded_dir)
  if handle then
    while true do
      local name, type = vim.uv.fs_scandir_next(handle)
      if not name then
        break
      end

      if type == "directory" then
        local full_path = expanded_dir .. "/" .. name
        for _, project in ipairs(as_projects(full_path)) do
          table.insert(projects, project)
        end
      end
    end
  end

  return projects
end

local function find_all_projects()
  local projects = { { path = vim.fn.expand "~/dotfiles", type = "git" } }
  local function add(dir)
    for _, project in ipairs(find_projects_in(dir)) do
      table.insert(projects, project)
    end
  end

  add "~/projects"
  add "~/ag"
  add "~/tmpdev"

  table.sort(projects, function(a, b)
    return a.path < b.path
  end)

  return projects
end

local function display(project)
  local prefix
  if project.type == "git" then
    prefix = "G"
  elseif project.type == "pijul" then
    prefix = "P"
  elseif project.type == "subversion" then
    prefix = "S"
  elseif project.type == "flake" then
    prefix = "F"
  elseif project.type == "worktree" then
    prefix = "T"
  end
  local home = vim.fn.getenv "HOME" .. "/"
  local path = vim.fn.substitute(project.path, home, "", "")
  return "[" .. prefix .. "]  " .. path
end

local function switch_project(_)
  local projects = find_all_projects()
  local entries = {}
  for _, project in ipairs(projects) do
    table.insert(entries, display(project))
  end

  if #entries == 0 then
    vim.notify("No projects found", vim.log.levels.WARN)
  end

  require("fzf-lua").fzf_exec(entries, {
    prompt = "Switch to project: ",
    actions = {
      ["default"] = function(selected)
        if selected and #selected > 0 then
          for _, project in ipairs(projects) do
            if display(project) == selected[1] then
              vim.cmd("cd " .. vim.fn.fnameescape(project.path))
              vim.notify("Switched project to " .. project.path)
            end
          end
        end
      end,
    },
  })
end

vim.api.nvim_create_user_command("SwitchProject", switch_project, {})
