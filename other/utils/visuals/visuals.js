const width = 600;
const height = 400;
const padding = 100;

const svg = d3.select("svg")
    .attr("width", width)
    .attr("height", height);


function horizontalizer(d) {
    if (d.group === "middle") {
        return width / 2;
    }
    if (d.group === "right") {
        return 3 * width / 4;
    }
    if (d.group === "frontend") {
        return width / 4;
    }
    if (d.group === "backend") {
        return 3 * width / 4;
    }
    return d.group === "backend" ? width / 4 : 3 * width / 4;
}

function labeler(d) {
    if (d.group === "middle") {
        return d.x;
    }
    if (d.group === "right") {
        return d.x - 10;
    }
    if (d.group === "frontend") {
        return d.x - 10;
    }
    if (d.group === "backend") {
        return d.x + 10;
    }
}


function loadAndDraw() {
    d3.json("/data.json").then(function (data) {
        const graphData = data;

        const backend = graphData.backend;
        const frontend = graphData.frontend;
        const other = graphData.other;

        const edges = graphData.edges;

        const nodes = [...backend, ...frontend, ...other];

        const links = edges.map(d => ({
            source: nodes.find(n => n.id === d.source),
            target: nodes.find(n => n.id === d.target)
        }));

        const simulation = d3.forceSimulation(nodes)
            .force("link", d3.forceLink(links).distance(100).strength(0.5))
            .force("charge", d3.forceManyBody().strength(-300))
            .force("center", d3.forceCenter(width / 2, height / 2))
            .force("x", d3.forceX().strength(0.1).x(d => horizontalizer(d)))
            .force("y", d3.forceY().strength(0.1).y(d => height / 2))
            .on("tick", ticked);

        const link = svg.selectAll(".link")
            .data(links)
            .enter().append("line")
            .attr("class", "link")
            .attr("stroke", "gray")
            .attr("stroke-width", 1.5);

        const node = svg.selectAll(".node")
            .data(nodes)
            //.enter().append("circle")
            .enter().append("svg:image")
            .attr("class", "node")
            .attr("xlink:href", "https://www.svgrepo.com/download/533811/donuts-cake.svg") // Path to the SVG icon file
            .attr("width", 20) // Adjust icon width
            .attr("height", 20) // Adjust icon height
            .attr("x", d => d.x)
            .attr("y", d => d.y)
            //.attr("r", 10)
            //.attr("fill", d => d.group === "backend" ? "steelblue" : "orange")
            .call(d3.drag()  // Enable dragging for interactivity
                .on("start", dragstarted)
                .on("drag", dragged)
                .on("end", dragended)
            );

        const label = svg.selectAll(".label")
            .data(nodes)
            .enter().append("text")
            .attr("class", "label")
            .attr("dy", 3)
            .attr("text-anchor", d => d.group === "backend" ? "start" : "end")
            .text(d => d.label);


        // Update positions on each tick of the simulation
        function ticked() {
            link
                .attr("x1", d => d.source.x)
                .attr("y1", d => d.source.y)
                .attr("x2", d => d.target.x)
                .attr("y2", d => d.target.y);

            node
                //.attr("cx", d => d.x)
                //.attr("cy", d => d.y);
                .attr("x", d => d.x - 10) // Adjust for icon center
                .attr("y", d => d.y - 10); // Adjust for icon center

            label
                .attr("x", d => labeler(d))
                .attr("y", d => d.y);
        }


        // Dragging functions to control node movement
        function dragstarted(event, d) {
            if (!event.active) simulation.alphaTarget(0.3).restart();
            d.fx = d.x;
            d.fy = d.y;
        }

        function dragged(event, d) {
            d.fx = event.x;
            d.fy = event.y;
        }

        function dragended(event, d) {
            if (!event.active) simulation.alphaTarget(0);
            d.fx = null;
            d.fy = null;
        }
    });
}


loadAndDraw();
