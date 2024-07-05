class Task {
    constructor(title, description, state, members, tag, subTasks) {
        this.title = title;
        this.description = description;
        this.state = state;
        this.members = members;
        this.tag = tag;
        this.subTasks = subTasks;
    }
}

class Member {
    constructor(name, role) {
        this.name = name;
        this.role = role;
    }
}

const makeTask = (title, description, state, tag) => {
    return new Task(title, description, state, [], tag, []);
};

const isFinished = (myTask) => {
    if (myTask.state == "DONE") return true;
    else return false;
};

const setDone = (task) => {
    return new Task(
        task.title,
        task.description,
        "DONE",
        task.members,
        task.tag,
        task.subTasks,
    );
};

const setDoneTasks = (list) => {
    return list.map(setDone);
};

const taskFromName = (title) => {
    return new Task(title, "", "BACKLOG", [], "TASK", []);
};

const createTasks = (titles) => {
    return titles.map(taskFromName);
};

const createTaskWithDescription = (title, state, member) => {
    if (state === "DONE" && member === "No Assigned") {
        return new Task(
            title,
            "This task was already finished with NoAssigned member",
            "DONE",
            [],
            "DefaultTag",
            [],
        );
    } else if (state === "IN PROGRESS") {
        return new Task(
            title,
            "This task is being developed",
            "IN PROGRESS",
            [],
            "DefaultTag",
            [],
        );
    } else {
        return new Task(title, "Default description", state, [], "DefaultTag", []);
    }
};

const createScrumProject = (projectTitle, projectDescription) => {
    return new Task(projectTitle, projectDescription, "No Assigned", [], "ProjectScrum", [
        new Task(
            "Sprint 1",
            "First sprint of the project",
            "No Assigned",
            [],
            "Sprint",
            [],
        ),
        new Task(
            "Sprint 2",
            "Second sprint of the project",
            "No Assigned",
            [],
            "Sprint",
            [],
        ),
        new Task(
            "Sprint 3",
            "Third sprint of the project",
            "NoAssigned",
            [],
            "Sprint",
            [],
        ),
    ]);
};

const main = () => {
    let project = createScrumProject(
        "My scrum project",
        "Lorem ipsum dolor sit amet consectetur adipisicing elit. Maxime mollitia, molestiae quas",
    );
    let tasks = createTasks(["first", "second", "thirs"]);
    let doneTasks = setDoneTasks(tasks);
    console.log(project);
    console.log(tasks);
    console.log(doneTasks);
};

main();
