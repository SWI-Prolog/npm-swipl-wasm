async function main() {
  let i = 0;

  while (true) {
    const res = await fetch(`https://api.github.com/repos/SWI-Prolog/swipl-devel/tags?page=${i++}&per_page=100`);
    const content = await res.json();

    console.log(content)

    if (!Array.isArray(content) || content.length === 0)
      break;
  }  
}

main()
