open Deque

let deq = ref(CatDeque.empty)

for (i in 1 to 10) {
  deq := CatDeque.cons(i, deq^)
}

deq := CatDeque.concat(deq^, deq^)

let text = ReasonReact.string


/*
let render_suspend = s => {
  let CatDeque.Susp(_labda, repr) = s;
  let render_repr = r => {
    switch (r) {
    |CatDeque.Lazy_empty => <div>{text("Empty")}</div>
    |CatDeque.Lazy_tail_force =>  
    }
  };
  <div> 
    {text("||")}
  </div>
}
*/

/*
let render_suspend = s => {
  let CatDeque.Susp(lambda, repr) = s;
  let v = lambda();
  <div>
     {text("middle")}
     {render_queue(v)}
  </div>
}
*/

let render_deque = q => {
  let rec render_cat : 'a.(('a=>ReasonReact.reactElement, CatDeque.cat('a))=>ReasonReact.reactElement) = (render_elem, q) => {
    let render_base = q => { // box already rendered by caller
      let elems = Array.map(render_elem, q);
      <div className="no-box">
        {if (Array.length(elems)==0) {
           text("[]");
         } else {
           ReasonReact.array(elems)}
         }
      </div>
    };
    

    switch (q) {
    |CatDeque.Shallow(arr) =>
      <div className="render-box content-member">
        <div className="title-box shallow">
          {text("Shallow")}
        </div>
        <div className="content-box">
          {render_base(arr)}
        </div>
      </div>
    |CatDeque.Deep(l, m, r) =>
      let CatDeque.Susp(lambda, repr) = m;
      let v = lambda();
      <div className="render-box content-member">
        <div className="title-box deep">
          {text("Deep")}
        </div>
        <div className="content-box">
          {render_base(l)}
          {render_cat(render_base, v)}
          {render_base(r)}
        </div>
      </div>
    }//switch


  };

  let render_number = i => <div className="int-box content-member">{text(string_of_int(i))}</div>;
  render_cat(render_number, q)
}

let component = ReasonReact.statelessComponent("Main");

let make = _children => {
    ...component,
    render: self =>
      <div className="root-box">
        {render_deque(deq^)}
      </div>
};
