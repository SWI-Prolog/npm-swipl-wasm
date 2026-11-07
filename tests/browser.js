const assert = require("assert");
const fs = require("fs");
const http = require("http");
const path = require("path");
const static = require("node-static");
const puppeteer = require("puppeteer");

function createServer() {
  return new Promise((resolve, reject) => {
    const file = new static.Server(path.join(__dirname, ".."));
    // eslint-disable-next-line no-undef
    server = http
      .createServer((request, response) => {
        request
          .addListener("end", () => {
            file.serve(request, response);
          })
          .resume();
      })
      .on("error", (error) => {
        reject(error);
      })
      .listen(8080, "0.0.0.0", () => {
        // eslint-disable-next-line no-undef
        resolve(server);
      });
  });
}

describe("SWI-Prolog WebAssembly on Browser", () => {
  it("should run browser version through Puppeteer", async () => {
    const server = await createServer();
    try {
      const browser = await puppeteer.launch({ headless: "new" });
      try {
        const page = await browser.newPage();
        page.setDefaultTimeout(1_5000);
        page.setDefaultNavigationTimeout(1_5000);
        await page.goto("http://localhost:8080/examples/browser.html");
        await page.waitForSelector("#solution");
        await page.waitForFunction(() => {
          const solutionElement = document.querySelector("#solution");
          return solutionElement.textContent === "a";
        });
      } finally {
        await browser.close();
      }
    } finally {
      server.close();
    }
  });

  it("should run the no-data web build from a saved state fetching only the wasm", async () => {
    // The no-data builds do not ship the Prolog library, so the page
    // boots from a saved state that we generate up front.
    const imagePath = path.join(__dirname, "..", "examples", "no-data-image.pvm");
    const SWIPL_BUNDLE = require("../dist/swipl/swipl-bundle");
    const bundle = await SWIPL_BUNDLE({
      arguments: ["-q", "-f", "prolog.pl"],
      preRun: [(module) => module.FS.writeFile("prolog.pl", "hello(world).\n")],
    });
    bundle.prolog.query('qsave_program("image.pvm")').once();
    fs.writeFileSync(imagePath, bundle.FS.readFile("/image.pvm"));

    const server = await createServer();
    try {
      const browser = await puppeteer.launch({ headless: "new" });
      try {
        const page = await browser.newPage();
        page.setDefaultTimeout(1_5000);
        page.setDefaultNavigationTimeout(1_5000);
        const requestedPaths = [];
        page.on("request", (request) => {
          requestedPaths.push(new URL(request.url()).pathname);
        });
        await page.goto("http://localhost:8080/examples/browser-no-data.html");
        await page.waitForSelector("#solution");
        await page.waitForFunction(() => {
          const solutionElement = document.querySelector("#solution");
          return solutionElement.textContent === "world";
        });
        assert.ok(
          requestedPaths.some((p) => p.endsWith("/swipl-web-no-data.wasm")),
          "the external wasm must be fetched"
        );
        assert.ok(
          requestedPaths.every((p) => !p.endsWith(".data")),
          "no .data file may be fetched"
        );
      } finally {
        await browser.close();
      }
    } finally {
      server.close();
      fs.rmSync(imagePath, { force: true });
    }
  });
});
