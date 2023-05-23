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
        page.setDefaultTimeout(5000);
        page.setDefaultNavigationTimeout(5000);
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
});
