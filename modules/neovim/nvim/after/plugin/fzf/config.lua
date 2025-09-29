local function is_project(path)
  local git_path = path .. "/.git"
  if vim.uv.fs_stat(git_path) then
    return true
  end

  return false
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
        if is_project(full_path) then
          table.insert(projects, {
            name = name,
            path = full_path,
            display = name .. " (" .. base_dir .. ")",
          })
        end
      end
    end
  end

  return projects
end

local function find_all_projects()
  local projects = { { name = "dotfiles", path = "~/dotfiles", display = "dotfiles" } }
  local function add(dir)
    for _, project in ipairs(find_projects_in(dir)) do
      table.insert(projects, project)
    end
  end

  add "~/projects"
  add "~/tmpdev"

  table.sort(projects, function(a, b)
    return a.name < b.name
  end)

  return projects
end

local function switch_project(_)
  local projects = find_all_projects()
  local entries = {}
  for _, project in ipairs(projects) do
    table.insert(entries, project.display)
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
            if project.display == selected[1] then
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
